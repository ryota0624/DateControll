module TimeEx exposing (..)

import Time exposing (..)
import DateControll exposing (..)

fromString : Zone -> String -> Maybe Posix
fromString zone str =
  let
    intDateElements = String.split "-" str
      |> List.filterMap (String.toInt)

    posixSeed = millisToPosix 0
  in
  case intDateElements of
    [year, month, day] -> Just (addDays day posixSeed)
    _ -> Nothing
