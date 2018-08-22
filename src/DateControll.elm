module DateControll exposing (..)

import Time exposing (Posix)

oneHourUnixtime : Int
oneHourUnixtime = 3600


modifyHour: (Int -> Int -> Int) -> Posix -> Posix
modifyHour operator basePosix =
  let
     baseMillis = Time.posixToMillis basePosix
  in
     Time.millisToPosix (operator baseMillis oneHourUnixtime)

addHour : Posix -> Posix
addHour basePosix = modifyHour ((+)) basePosix

subtractHour : Posix -> Posix
subtractHour basePosix = modifyHour ((-)) basePosix

modifyHours : (Int -> Int -> Int) -> Int -> Posix -> Posix
modifyHours operator hourCount basePosix =
  if hourCount == 0 then basePosix
  else if hourCount > 0 then modifyHours operator (hourCount - 1) basePosix
  else modifyHours operator (hourCount + 1) basePosix

addHours : Int -> Posix -> Posix
addHours hourCount basePosix =
    modifyHours ((+)) hourCount basePosix

substractHours : Int -> Posix -> Posix
substractHours hourCount basePosix =
    modifyHours ((-)) hourCount basePosix

addDays : Int -> Posix -> Posix
addDays dayCount basePosix = addHours (24 * dayCount) basePosix

substractDays : Int -> Posix -> Posix
substractDays dayCount basePosix = substractHours (24 * dayCount) basePosix
