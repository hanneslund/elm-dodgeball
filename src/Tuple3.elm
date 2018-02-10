module Tuple3 exposing (first, second, third)


first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a


second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c
