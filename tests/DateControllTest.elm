module DateControllTest exposing (addDaysSpec, addHourSpec, addHoursSpec, posixSeed, substractDaysSpec, substractHoursSpec)

import DateControll exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (..)


posixSeed : Posix
posixSeed =
    millisToPosix 0


addHourSpec : Test
addHourSpec =
    describe "addHour"
        [ test "apply addHour" <| \_ -> Expect.equal (addHour posixSeed) (millisToPosix 3600)
        ]


addHoursSpec : Test
addHoursSpec =
    describe "addHours"
        [ fuzz int "apply number addHours" <|
            \randomInt ->
                Expect.equal (addHours randomInt posixSeed) (millisToPosix (3600 * randomInt))
        ]


substractHoursSpec : Test
substractHoursSpec =
    describe "substractHours"
        [ fuzz int "apply number substractHours" <|
            \randomInt ->
                Expect.equal (substractHours randomInt posixSeed) (millisToPosix (3600 * -randomInt))
        ]


addDaysSpec : Test
addDaysSpec =
    describe "addDays"
        [ fuzz int "apply number addDays" <|
            \randomInt ->
                Expect.equal (addDays randomInt posixSeed) (millisToPosix (3600 * randomInt * 24))
        ]


substractDaysSpec : Test
substractDaysSpec =
    describe "substractDays"
        [ fuzz int "apply number substractDays" <|
            \randomInt ->
                Expect.equal (substractDays randomInt posixSeed) (millisToPosix (3600 * -randomInt * 24))
        ]
