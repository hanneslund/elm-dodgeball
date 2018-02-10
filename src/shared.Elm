module Shared exposing ((?++), maxsize, minsize)


(?++) : List a -> Maybe a -> List a
(?++) list mby =
    case mby of
        Just a ->
            list ++ [ a ]

        Nothing ->
            list


infixr 5 ?::


minsize : Int
minsize =
    1


maxsize : Int
maxsize =
    7
