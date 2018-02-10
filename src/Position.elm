module Position exposing (Position, asTuple, move, position)

import Shared exposing (maxsize, minsize)


type alias X =
    Int


type alias Y =
    Int


type Position
    = Position X Y


position : X -> Y -> Maybe Position
position x y =
    if x < minsize || x > maxsize || y < minsize || y > maxsize then
        Nothing
    else
        Just <| Position x y


move : X -> Y -> Position -> Maybe Position
move x2 y2 (Position x1 y1) =
    position (x1 + x2) (y1 + y2)


asTuple : Position -> ( X, Y )
asTuple (Position x y) =
    ( x, y )
