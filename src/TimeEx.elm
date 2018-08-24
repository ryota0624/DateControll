module TimeEx exposing (formatYMd, formatYMdHm, fromString, fromYMd, monthFromInt, toIntMonth)

{-| 
    @docs formatYMd, formatYMdHm, fromString, fromYMd, monthFromInt, toIntMonth
|-}
import DateControll exposing (..)
import Time exposing (..)
import Iso8601 as Iso

{-| |-}
toDoubleDigitString : Int -> String
toDoubleDigitString day =
    if 0 <= day && day < 10 then
        "0" ++ String.fromInt day

    else
        String.fromInt day

{-| |-}
formatYMd : Zone -> Posix -> String
formatYMd zone posix =
    let
        year =
            toYear zone posix

        month =
            toMonth zone posix |> toIntMonth

        day =
            toDay zone posix
    in
    [ year, month, day ] |> List.map String.fromInt |> String.join "-"

{-| |-}
formatYMdHm : Zone -> Posix -> String
formatYMdHm zone posix =
    let
        hour =
            toHour zone posix |> String.fromInt
        minute =
            toMinute zone posix |> String.fromInt
    in
    formatYMd zone posix ++ " " ++ hour ++ ":" ++ minute

{-| |-}
fromYMd : Zone -> Int -> Month -> Int -> Maybe Posix
fromYMd zone year month day =
    [ year |> String.fromInt, month |> toIntMonth |> toDoubleDigitString, day |> toDoubleDigitString ]
        |> String.join "-" |> fromString zone

{-| |-}
fromString : Zone -> String -> Maybe Posix
fromString zone str = Iso.toTime (str ++ "T00:00:00.000Z") |> Result.toMaybe
    

{-| |-}
toIntMonth : Month -> Int
toIntMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12

{-| |-}
monthFromInt : Int -> Maybe Month
monthFromInt month =
    case month of
        1 ->
            Jan |> Just

        2 ->
            Feb |> Just

        3 ->
            Mar |> Just

        4 ->
            Apr |> Just

        5 ->
            May |> Just

        6 ->
            Jun |> Just

        7 ->
            Jul |> Just

        8 ->
            Aug |> Just

        9 ->
            Sep |> Just

        10 ->
            Oct |> Just

        11 ->
            Nov |> Just

        12 ->
            Dec |> Just

        _ ->
            Nothing
