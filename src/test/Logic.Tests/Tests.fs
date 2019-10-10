module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events 
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests on several days overlapsing on a half a day with request1 before request 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
      }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
      }
      
      Expect.isTrue (Logic.overlapsWith request1 request2) "The request should overlaps"
    }
    
    test "Requests on several days overlapsing on a half a day with request1 after request 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
      }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
      }
      
      Expect.isTrue (Logic.overlapsWith request2 request1) "The request should overlaps"
    }
    
    test "Requests overlaps with a set of requests" {
      let newRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
      }
      
      let existingRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 10); HalfDay = AM }
          End = { Date = DateTime(2019, 09, 11); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
          End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
        }
      ]
      
      let existingRequestsAsSeq = Seq.ofList existingRequests
      
      Expect.isTrue (Logic.overlapsWithAnyRequest existingRequestsAsSeq newRequest)
        "The request should overlaps with the sequence"
    }
    
    test "Requests don't overlaps with a set of requests" {
      let newRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 12); HalfDay = PM }
      }
      
      let existingRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 10); HalfDay = AM }
          End = { Date = DateTime(2019, 09, 11); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
          End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
        }
      ]
      
      let existingRequestsAsSeq = Seq.ofList existingRequests
      
      Expect.isFalse (Logic.overlapsWithAnyRequest existingRequestsAsSeq newRequest)
        "The request shouldn't overlaps with the sequence"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
  ]
  
[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let denyTests =
  testList "Validation tests" [
    test "A request is denied" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (DenyRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestDenied request]) "The request should have been denied"
    }
    
    test "Employee can't deny his request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (DenyRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should have been denied by a manager"
    }
  ]
