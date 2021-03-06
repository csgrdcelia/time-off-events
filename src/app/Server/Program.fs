module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }
    
    [<CLIMutable>]
    type UserAndDate = {
        UserId: UserId
        Date: DateTime
    }

    let requestTimeOff (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! bindedTOR = ctx.BindJsonAsync<TimeOffRequest>()
                let timeOffRequest = {UserId = bindedTOR.UserId;
                                      RequestId = Guid.NewGuid();
                                      Start = bindedTOR.Start;
                                      End = bindedTOR.End;
                                      TreatmentDate = DateTime.Now}
                
                let command = RequestTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
     
    let cancelRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestCancelled timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
            
    let denyRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun(next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = DenyRequest(userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestDenied timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
            
    let getLeaveBalance (getStateOf: UserId -> Logic.UserRequestsState) =
        fun(next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndDate = ctx.BindQueryString<UserAndDate>()
                let state = getStateOf userAndDate.UserId
                let result = Logic.getLeaveBalance userAndDate.Date state
                match result with
                | Ok request -> return! json request next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    let getHistory (getEventsOf: UserId -> seq<RequestEvent>) =
        fun(next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndDate = ctx.BindQueryString<UserAndDate>()
                let result = getEventsOf userAndDate.UserId
                return! json result next ctx
            }
            
    let askForCancellation(handleCommand: Command -> Result<RequestEvent list, string>) =
        fun(next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = AskForCancellation(userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestPendingCancellation timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =
    let getEvents userId =
        let eventStream = eventStore.GetStream(userId)
        eventStream.ReadAll()
    
    let getState userId = getEvents userId |> Seq.fold Logic.evolveUserRequests Map.empty
        
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId
        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.decide DateTime.Today state user command
        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result
        
    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=> route "/request" >=> HttpHandlers.requestTimeOff (handleCommand user)
                            POST >=> route "/validate-request" >=> HttpHandlers.validateRequest (handleCommand user)
                            POST >=> route "/cancel-request" >=> HttpHandlers.cancelRequest (handleCommand user)
                            POST >=> route "/deny-request" >=> HttpHandlers.denyRequest (handleCommand user)
                            POST >=> route "/ask-for-cancellation" >=> HttpHandlers.askForCancellation(handleCommand user)
                            GET >=> route "/balance" >=> HttpHandlers.getLeaveBalance (getState)
                            GET >=> route "/history" >=> HttpHandlers.getHistory(getEvents)
                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "..\..\..\.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%s")
    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0