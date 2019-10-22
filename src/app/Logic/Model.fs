namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | AskForCancellation of UserId * Guid
    | DenyRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | AskForCancellation (userId, _) -> userId
        | DenyRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestDenied of TimeOffRequest
    | RequestPendingCancellation of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
        | RequestDenied request -> request
        | RequestPendingCancellation request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | Cancelled of TimeOffRequest
        | PendingValidation of TimeOffRequest
        | PendingCancellation of TimeOffRequest
        | Validated of TimeOffRequest
        | Denied of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Cancelled request -> request
            | PendingValidation request
            | PendingCancellation request
            | Validated request -> request
            | Denied request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Cancelled _ -> true
            | PendingValidation _
            | PendingCancellation _ -> true
            | Validated _ -> true
            | Denied _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelled request -> Cancelled request
        | RequestPendingCancellation request -> PendingCancellation request
        | RequestDenied request -> Denied request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)
    
    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest)  =
        request1.Start <= request2.Start && request1.End >= request2.Start ||
        request1.Start <= request2.End && request1.End >= request2.End

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        Seq.exists (overlapsWith request) otherRequests 

    let createRequest activeUserRequests currentDate request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= currentDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
            
    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | Validated request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request already cancelled"
            
    let denyRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestDenied request]
        | _ ->
            Error "Request cannot be denied"
            
    let askForCancellation requestState =
        match requestState with
        | Validated request ->
            Ok [RequestPendingCancellation request]
        | _ ->
            Error "Request cannot be pending cancellation"

    let decide (currentDate: DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests currentDate request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                if currentDate >= requestState.Request.Start.Date then
                    Error "The request has begun or begins this afternoon"
                else if (user = Manager && (userRequests.TryFind requestId).Value = Validated (userRequests.TryFind requestId).Value.Request ) || user = Employee relatedUserId then
                    cancelRequest requestState
                else
                    Error "Unauthorized"
            | AskForCancellation (_, requestId) ->
                if (user <> Employee relatedUserId) then
                    Error "Unauthorized"
                else 
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    if currentDate < requestState.Request.Start.Date then
                        Error "The request can be cancelled directly"
                    else
                        askForCancellation requestState
            | DenyRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    denyRequest requestState
