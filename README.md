# elm-time [![Build Status](https://travis-ci.org/Bogdanp/elm-time.svg)](https://travis-ci.org/Bogdanp/elm-time)

``` shell
elm package install Bogdanp/elm-time
```

## Dates

`Date`s may represent any date in the [proleptic Gregorian calendar][cal].

``` elm
import Time.Date as Date exposing (Date, date)
```

### Constructing Dates

Use `date` to construct `Date` values.  If given a valid date, the
function returns `Just` that date, otherwise it returns `Nothing`.

``` elm
> date 1992 2 28
Just (Date { year = 1992, month = 2, day = 28 }) : Maybe Date

> date 1992 2 31
Nothing : Maybe Date
```

``` elm
> let
|   birthday : Maybe Date
|   birthday = date 1992 5 29
| in
|   Maybe.map Date.toString birthday
Just "1992-05-29"
```

Use `year`, `month`, and `day` to inspect `Date`s.

``` elm
> Maybe.map Date.year birthday
Just 1992 : Maybe Int

> Maybe.map Date.month birthday
Just 5 : Maybe Int

> Maybe.map Date.day birthday
Just 29 : Maybe Int
```

### Manipulating Dates

`FIXME`

### Comparing Dates

`FIXME`

## DateTimes

`DateTimes` represent a `Date` together with a time offset from midnight.

``` elm
import Time.DateTime as DateTime exposing (DateTime, dateTime)
```

### Constructing DateTimes

`DateTime`s can be constructed from a record using the `dateTime`
function or from a UTC timestamp in milliseconds using
`fromTimestamp`.

To construct a `DateTime` using `dateTime`, pass it a record
containing fields for `year`, `month`, `day`, `hour`, `minute`,
`second`, `millisecond`:

``` elm
> dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
Just (DateTime { date = Date { year = 1992, month = 5, day = 29 }, offset = 0 }) : Maybe Date

> dateTime { year = 1992, month = 2, day = 31, hour = 0, minute = 0, second = 0, millisecond = 0 }
Nothing : Maybe Date
```

To make constructing `DateTimes` a little easier, the library provides
`Time.DateTime.zero`:

``` elm
> import Time.DateTime as DateTime exposing (DateTime, dateTime, zero)

> dateTime { zero | year = 1992 }
|   > Maybe.map toISO8601
Just "1992-01-01T00:00:00Z"

> dateTime { zero | year = 1992, month = 2, day = 28, hour = 5 }
|   > Maybe.map toISO8601
Just "1992-02-28T05:00:00Z"
```

Use `fromTimestamp` to construct a `DateTime` from a UTC timestamp in
milliseconds:

``` elm
> fromTimestamp 0
|   > toISO8601
"1970-01-01T00:00:00Z"
```

See `examples/without-timezone` for an example of how to construct
`DateTime`s from local time.

### Manipulating DateTimes

`FIXME`

### Comparing DateTimes

`FIXME`

### Helpers

`FIXME`

## ZonedDateTimes

`ZonedDateTimes` represent a `DateTime` in a specific `TimeZone`.

``` elm
ipmort Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime)
```


[cal]: https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
