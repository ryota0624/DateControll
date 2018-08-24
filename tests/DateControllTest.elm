module DateControllTest exposing (addDaysSpec, addHourSpec, addHoursSpec, posixSeed, substractDaysSpec, substractHoursSpec)

import DateControll exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (..)
import TimeEx exposing (..)

posixSeed : Posix
posixSeed =
    millisToPosix 0


addHourSpec : Test
addHourSpec =
    describe "addHour"
        [ test "apply addHour" <| \_ -> Expect.equal (addHour posixSeed) (millisToPosix 3600000)
        , test "apply addHour get hour" <| \_ -> 
                Expect.equal (addHour posixSeed |> Time.toHour utc) (1)
        ]


addHoursSpec : Test
addHoursSpec =
    describe "addHours"
        [ fuzz int "apply number addHours" <|
            \randomInt ->
                Expect.equal (addHours randomInt posixSeed) (millisToPosix (3600000 * randomInt))
          , fuzz int "apply number addHours get hour" <|
            \randomInt ->
                let
                    posix = addHours randomInt posixSeed
                in
                    if (randomInt >= 0 && randomInt < 24) then 
                        Expect.equal (posix |> Time.toHour utc) (randomInt)
                    else Expect.true "NoTestCase" True
        ]


substractHoursSpec : Test
substractHoursSpec =
    describe "substractHours"
        [ fuzz int "apply number substractHours" <|
            \randomInt ->
                Expect.equal (substractHours randomInt posixSeed) (millisToPosix (3600000 * -randomInt))
        ]


addDaysSpec : Test
addDaysSpec =
    describe "addDays"
        [ fuzz int "apply number addDays" <|
            \randomInt ->
                Expect.equal (addDays randomInt posixSeed) (millisToPosix (3600000 * randomInt * 24))
          , fuzz int "apply number adDays get day" <|
            \randomInt ->
                let
                    posix = addDays randomInt posixSeed
                in
                    if (randomInt >= 0 && randomInt <= 30) then
                        Expect.equal (posix |> Time.toDay utc) (randomInt + 1)
                    else Expect.true "NoTestCase" True                
        ]


substractDaysSpec : Test
substractDaysSpec =
    describe "substractDays"
        [ fuzz int "apply number substractDays" <|
            \randomInt ->
                Expect.equal (substractDays randomInt posixSeed) (millisToPosix (3600000 * -randomInt * 24))
        ]
