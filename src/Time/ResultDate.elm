module Time.ResultDate
    exposing
        ( Date
        , Day
        , Month(..)
        , Year
        , createYear
        , createMonth
        , createDay
        , createDate
        , isLeapYear
        )


type Date
    = Date Year Month Day


type Year
    = Year Int


type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


type Day
    = Day Int


createYear : Int -> Result String Year
createYear year =
    if year < 1583 then
        Err "ISO 8601 does not handle years before 1583"
    else if year > 9999 then
        Err "ISO 8601 does not handle years after 9999"
    else
        Ok <| Year year


createMonth : Int -> Result String Month
createMonth month =
    case month of
        1 ->
            Ok January

        2 ->
            Ok February

        3 ->
            Ok March

        4 ->
            Ok April

        5 ->
            Ok May

        6 ->
            Ok June

        7 ->
            Ok July

        8 ->
            Ok August

        9 ->
            Ok September

        10 ->
            Ok October

        11 ->
            Ok November

        12 ->
            Ok December

        m ->
            Err <| "There is no month that maps to " ++ toString m


createDay : Year -> Month -> Int -> Result String Day
createDay year month day =
    if day < 0 then
        Err "Negative integers are not a valid day"
    else if day == 0 then
        Err "Zero is not a valid day"
    else if day > 31 then
        Err "Any month has at most thirty-one days"
    else if day == 31 && not (hasThirtyOneDays month) then
        Err <| monthToString month ++ " does not have thirty-one days"
    else if day == 30 && not (hasThirtyDays month) then
        Err <| monthToString month ++ " does not have thirty days"
    else if day == 29 && month == February && not (isLeapYear year) then
        Err "February only have twenty-nine days during leap years"
    else
        Ok <| Day day


createDate : Int -> Int -> Int -> Result String Date
createDate year month day =
    let
        yearResult : Result String Year
        yearResult =
            createYear year

        monthResult : Result String Month
        monthResult =
            createMonth month

        dayResult : Result String Day
        dayResult =
            case yearResult of
                Ok year ->
                    case monthResult of
                        Ok month ->
                            createDay year month day

                        Err err ->
                            Err err

                Err err ->
                    Err err
    in
        Ok Date
            |> andMap yearResult
            |> andMap monthResult
            |> andMap dayResult


isLeapYear : Year -> Bool
isLeapYear (Year year) =
    year % 400 == 0 || year % 100 /= 0 && year % 4 == 0


hasThirtyDays : Month -> Bool
hasThirtyDays month =
    month == April || month == June || month == September || month == November


hasThirtyOneDays : Month -> Bool
hasThirtyOneDays month =
    month /= February || not (hasThirtyDays month)


monthToString : Month -> String
monthToString month =
    case month of
        January ->
            "January"

        February ->
            "February"

        March ->
            "March"

        April ->
            "April"

        May ->
            "May"

        June ->
            "June"

        July ->
            "July"

        August ->
            "August"

        September ->
            "September"

        October ->
            "October"

        November ->
            "November"

        December ->
            "December"


andMap : Result x a -> Result x (a -> b) -> Result x b
andMap =
    Result.map2 (|>)
