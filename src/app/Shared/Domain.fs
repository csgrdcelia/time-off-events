﻿namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
    TreatmentDate: DateTime
}

[<CLIMutable>]
type LeaveBalance = {
    GrantedLeave: float
    CarriedLeave: float
    TakenLeave: float
    PlannedLeave: float
    CurrentBalance: float
}
