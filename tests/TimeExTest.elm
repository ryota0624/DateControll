module TimeExTest exposing (fromYMdSpec)

import DateControll exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (..)
import TimeEx exposing (..)


posixSeed : Posix
posixSeed =
    millisToPosix 0


fromYMdSpec : Test
fromYMdSpec =
    describe "fromYMd"
        [ test "apply fromYMd" <| \_ -> 
          Expect.equal 
            (fromYMd utc 2020 Dec 2 |> Maybe.map (formatYMd utc)) 
            ("2020-12-2" |> Just)
        ]