module TimeOff.Tests

open Expecto
open System

let currentDate = DateTime(2019, 10, 1)
let Given(events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add(event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide currentDate userRequestsState user command
    Expect.equal result expected message

[<Tests>]
let overlapTests =
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Requests on several days overlapsing on a half a day with request1 before request 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The request should overlaps"
    }

    test "Requests on several days overlapsing on a half a day with request1 after request 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.isTrue (Logic.overlapsWith request2 request1) "The request should overlaps"
    }

    test "Requests overlaps with a set of requests" {
      let newRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 12); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 13); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      let existingRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 10); HalfDay = AM }
          End = { Date = DateTime(2019, 09, 11); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
          End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
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
        TreatmentDate = DateTime(2019, 01, 02)
      }

      let existingRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 10); HalfDay = AM }
          End = { Date = DateTime(2019, 09, 11); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 13); HalfDay = PM }
          End = { Date = DateTime(2019, 09, 14); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
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
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given []
      |> ConnectedAs(Employee "jdoe")
      |> When(RequestTimeOff request)
      |> Then (Ok [ RequestCreated request ]) "The request should have been created"
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
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When(ValidateRequest("jdoe", request.RequestId))
      |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
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
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When(DenyRequest("jdoe", request.RequestId))
      |> Then (Ok [ RequestDenied request ]) "The request should have been denied"
    }

    test "Employee can't deny his request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(DenyRequest("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should have been denied by a manager"
    }
  ]

[<Tests>]
let cancelTests =
  testList "Cancel tests" [
    test "A request is cancelled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should have been cancelled"
    }

    test "A request can't be cancelled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "other")
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The user should not be authorized to cancel the request"
    }

    test "A request already cancelled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCancelled request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Error "Request already cancelled") "The request should have been already cancelled"
    }

    test "Manager can cancel the validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should have been cancelled"
    }

    test "Manager can't cancel not validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been cancelled"
    }

    test "User can't cancel started request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Error "The request has begun or begins this afternoon") "The user should ask for cancellation"
    }

    test "User can't cancel request that start on the afternoon" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 01); HalfDay = PM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(CancelRequest("jdoe", request.RequestId))
      |> Then (Error "The request has begun or begins this afternoon") "The user should ask for cancellation"
    }
  ]

[<Tests>]
let askForCancellationTests =
  testList "Ask for cancellation tests" [
    test "Ask for cancellation of a started validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 30); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestValidated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Ok [ RequestPendingCancellation request ]) "The request should be waiting for cancellation"
    }

    test "Ask for cancellation of a validated request that starts in the afternoon" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 01); HalfDay = PM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestValidated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Ok [ RequestPendingCancellation request ]) "The request should be waiting for cancellation"
    }

    test "Ask for cancellation of a started but not validated request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 30); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Error "Request cannot be pending cancellation") "Request cannot be pending cancellation because it has not been validated"
    }

    test "Ask for cancellation of a started but denied request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 30); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestDenied request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Error "Request cannot be pending cancellation") "Request cannot be pending cancellation because it was denied"
    }

    test "Ask for cancellation of a not started request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 15); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 30); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestValidated request ]
      |> ConnectedAs(Employee "jdoe")
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Error "The request can be cancelled directly") "The request can be cancelled directly"
    }

    test "Ask for cancellation by manager of a started request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 30); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When(AskForCancellation("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The manager is unauthorized"
    }

  ]

[<Tests>]
let timeOffCountTests =
  testList "Count days of a TimeOff request" [
    test "Unique day time off" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 02); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.equal (Logic.countTimeOffDuration request) 1. "Unique day time off should be equal to 1"
    }

    test "Couple days time off" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 03); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.equal (Logic.countTimeOffDuration request) 2. "Couple days time off should be equal to 2"
    }

    test "Half day time off" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.equal (Logic.countTimeOffDuration request) 0.5 "Half day time off should be equal to 0.5"
    }

    test "One and a half days time off" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.equal (Logic.countTimeOffDuration request) 1.5 "One and a half days time off should be equal to 1.5"
    }

    test "Week time off" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 08); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }

      Expect.equal (Logic.countTimeOffDuration request) 5. "Week time off should be equal to 5"
    }
    
    test "Week time off with a public holiday" {
      let request = {
        UserId = "jeod"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 23); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
        TreatmentDate = DateTime(2019, 01, 02)
      }
      
      Expect.equal (Logic.countTimeOffDuration request) 4. "Should be equal to 4"
    }
  ]

[<Tests>]
let leaveBalanceTests =
  testList "Test leave balance" [
    test "Granted leave in october" {
      let requests = Map.empty
      let result =
        match (Logic.getLeaveBalance (DateTime(2019, 11, 3)) requests) with
        | Ok balance -> balance
        | Error error -> error
      Expect.equal result.GrantedLeave 25.0 "Granted leave should be equal to 25"
    }
    test "Granted leave in january" {
      let requests = Map.empty
      let result =
        match (Logic.getLeaveBalance (DateTime(2020, 1, 3)) requests) with
        | Ok balance -> balance
        | Error error -> error
      Expect.equal result.GrantedLeave 0.0 "Granted leave should be equal to 0"
    }
    
    test "8 taken leave" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 13); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 17); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 21); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 23); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.getTakenLeave (DateTime(2020, 02, 05)) activeRequests
      Expect.equal result 8.0 "Taken leave should be equal to 8"
    }
    
    test "1 taken leave" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 02); HalfDay = AM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.getTakenLeave (DateTime(2020, 02, 05)) activeRequests
      Expect.equal result 1.0 "Taken leave should be equal to 1"
    }
    
    test "3.5 taken leave because 2020, 01, 04 is saturday" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 02); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 04); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.getTakenLeave (DateTime(2020, 02, 05)) activeRequests
      Expect.equal result 2.5 "Taken leave should be equal to 2.5 because 2020, 01, 04 is saturday"
    }

    
    test "Half day planned leave" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 01); HalfDay = AM }
          End = { Date = DateTime(2019, 01, 04); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.getPlannedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result 0.5 "Planned leave should be equal to 0.5"
    }
    
    test "Multiple days planned leave" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 01); HalfDay = AM }
          End = { Date = DateTime(2019, 01, 04); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 01, 06); HalfDay = AM }
          End = { Date = DateTime(2020, 01, 10); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.getPlannedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result 5.0 "Planned leave should be equal to 5"
    }
    
    test "No carried leaves" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 02); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 11); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.calculateCarriedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result 0.0 "Carried leave should be equal to 0"
    }
    
    test "30 carried leaves" {
      let activeRequests = []
      let result = Logic.calculateCarriedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result 30.0 "Carried leave should be equal to 30"
    }
    
    test "14.5 carried leaves" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 02); HalfDay = AM }
          End = { Date = DateTime(2019, 09, 23); HalfDay = AM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.calculateCarriedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result 14.5 "Carried leave should be equal to 14.5"
    }
    
    test "-5 carried leaves" {
      let activeRequests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 09, 02); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 11); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 14); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 18); HalfDay = PM }
          TreatmentDate = DateTime(2019, 01, 02)
        }
      ]
      let result = Logic.calculateCarriedLeave (DateTime(2020, 01, 05)) activeRequests
      Expect.equal result -5.0 "Carried leave should be equal to -5"
    }
  ]
