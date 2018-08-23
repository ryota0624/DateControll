module DateControll exposing (Year, addDays, addHour, addHours, daysInMonth, isLeapYear, modifyHour, modifyHours, oneHourUnixtime, substractDays, substractHours, subtractHour)

import Time exposing (..)


oneHourUnixtime : Int
oneHourUnixtime =
    3600


modifyHour : (Int -> Int -> Int) -> Posix -> Posix
modifyHour operator basePosix =
    let
        baseMillis =
            Time.posixToMillis basePosix
    in
    Time.millisToPosix (operator baseMillis oneHourUnixtime)


addHour : Posix -> Posix
addHour basePosix =
    modifyHour (+) basePosix


subtractHour : Posix -> Posix
subtractHour basePosix =
    modifyHour (-) basePosix


modifyHours : Int -> Posix -> Posix
modifyHours hourCount basePosix =
    let
        baseMillis =
            Time.posixToMillis basePosix
    in
    if hourCount == 0 then
        basePosix

    else
        Time.millisToPosix (baseMillis + (oneHourUnixtime * hourCount))



addHours : Int -> Posix -> Posix
addHours hourCount basePosix =
    modifyHours hourCount basePosix


substractHours : Int -> Posix -> Posix
substractHours hourCount basePosix =
    modifyHours (negate hourCount) basePosix


addDays : Int -> Posix -> Posix
addDays dayCount basePosix =
    addHours (24 * dayCount) basePosix


substractDays : Int -> Posix -> Posix
substractDays dayCount basePosix =
    substractHours (24 * dayCount) basePosix


type alias Year =
    Int


isLeapYear : Year -> Bool
isLeapYear y =
    remainderBy y 400 == 0 || remainderBy y 100 /= 0 && remainderBy y 4 == 0


daysInMonth : Year -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31
