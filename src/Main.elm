port module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Keyed as Keyed
import Keyboard exposing (KeyCode)
import List.Extra as List
import Maybe.Extra as Maybe exposing ((?))
import Mouse
import Position exposing (Position, position)
import Random
import Shared exposing ((?++), maxsize, minsize)
import Time exposing (Time)
import Tuple3


-- Model


type alias Player =
    Maybe Position


type GameState
    = Playing
    | CantRespawn
    | Respawnable


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Key =
    Int


type alias Enemy =
    ( Position, Direction, Key )


type alias Model =
    { state : GameState
    , player : Player
    , playerCanMove : Bool
    , aim : Direction
    , score : Int
    , highscore : Int
    , enemies : List Enemy
    }



-- Update


randomEnemy : Key -> Random.Generator (Maybe Enemy)
randomEnemy key =
    let
        toEnemy side i =
            let
                getEnemy dir =
                    Maybe.map (\pos -> ( pos, dir, key ))
            in
            case side of
                1 ->
                    getEnemy Down <| position i minsize

                2 ->
                    getEnemy Up <| position i maxsize

                3 ->
                    getEnemy Left <| position maxsize i

                _ ->
                    getEnemy Right <| position minsize i
    in
    Random.map2
        toEnemy
        (Random.int 1 4)
        (Random.int minsize maxsize)


type Msg
    = MoveEnemies Time
    | SpawnEnemy Time
    | SpawnedEnemy (Maybe Enemy)
    | MoveAim Time
    | KeyDown KeyCode
    | MouseDown Mouse.Position
    | SetRespawnable Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Playing ->
            step msg model

        CantRespawn ->
            case msg of
                SetRespawnable _ ->
                    ( { model | state = Respawnable }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Respawnable ->
            let
                respawn =
                    { init
                        | state = Playing
                        , highscore = model.highscore
                    }
            in
            case msg of
                MouseDown _ ->
                    ( respawn, Cmd.none )

                KeyDown keyCode ->
                    ( ifSpaceDown keyCode respawn model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    let
        nextModel =
            case msg of
                MoveEnemies _ ->
                    let
                        movedEnemies =
                            List.map moveEnemy model.enemies |> Maybe.values

                        despawnedEnemiesCount =
                            List.length model.enemies - List.length movedEnemies
                    in
                    { model
                        | enemies = movedEnemies
                        , score = model.score + despawnedEnemiesCount
                    }

                SpawnEnemy _ ->
                    model

                SpawnedEnemy mbyEnemy ->
                    { model | enemies = model.enemies ?++ mbyEnemy }

                MoveAim _ ->
                    { model
                        | playerCanMove = True
                        , aim = moveAim model.aim
                    }

                KeyDown keyCode ->
                    ifSpaceDown keyCode (onMove model) model

                MouseDown _ ->
                    onMove model

                SetRespawnable _ ->
                    model

        command =
            case msg of
                SpawnEnemy _ ->
                    let
                        nextKey =
                            List.last model.enemies
                                |> Maybe.unwrap 0 (Tuple3.third >> (+) 1)
                    in
                    Random.generate SpawnedEnemy (randomEnemy nextKey)

                _ ->
                    Cmd.none
    in
    if checkCollision nextModel.enemies nextModel.player then
        let
            highscore =
                max nextModel.score nextModel.highscore
        in
        ( { nextModel
            | state = CantRespawn
            , highscore = highscore
            , player = Nothing
          }
        , saveHighscore highscore
        )
    else
        ( nextModel, command )


ifSpaceDown : KeyCode -> a -> a -> a
ifSpaceDown keyCode ifSpace default =
    if Char.fromCode keyCode == ' ' then
        ifSpace
    else
        default


onMove : Model -> Model
onMove model =
    if
        model.playerCanMove
            && Maybe.isJust (aimPosition model)
    then
        { model
            | playerCanMove = False
            , player = Maybe.andThen (getPosFromDir model.aim) model.player
        }
    else
        model


moveEnemy : Enemy -> Maybe Enemy
moveEnemy ( oldPos, dir, key ) =
    Maybe.map (\pos -> ( pos, dir, key )) (getPosFromDir dir oldPos)


getPosFromDir : Direction -> Position -> Maybe Position
getPosFromDir dir =
    case dir of
        Up ->
            Position.move 0 -1

        Down ->
            Position.move 0 1

        Left ->
            Position.move -1 0

        Right ->
            Position.move 1 0


checkCollision : List Enemy -> Player -> Bool
checkCollision enemies player =
    List.map (Tuple3.first >> Just) enemies
        |> List.member player


moveAim : Direction -> Direction
moveAim aim =
    case aim of
        Left ->
            Up

        Up ->
            Right

        Right ->
            Down

        Down ->
            Left


aimPosition : Model -> Maybe Position
aimPosition model =
    Maybe.andThen (getPosFromDir model.aim) model.player



-- View


view : Model -> Html Msg
view model =
    let
        textClasses =
            "text-center text-white uppercase text-shadow m-4"

        floorClasses =
            "bg-grey-darker border-4 border-grey-darkest m-px"

        tiles i =
            List.repeat i (tile "orange")

        floor i =
            List.repeat i (div [ class floorClasses ] [])

        keyedPlayer =
            Maybe.map (player >> (,) "player") model.player

        keyedAim =
            aimPosition model
                |> Maybe.map (aim model.playerCanMove >> (,) "aim")
    in
    div
        [ classList
            [ ( "width m-auto px-2", True )
            , ( "shake", Maybe.isNothing model.player )
            ]
        ]
        [ h2 [ class (textClasses ++ " text-5xl") ] [ text <| toString model.score ]
        , div [ class "game" ] <|
            tiles 10
                ++ [ div [ class "relative" ] <|
                        div [ class <| floorClasses ++ " absolute pin" ] []
                            :: [ Keyed.node "div" [ class "absolute w-full" ] <|
                                    List.map enemy model.enemies
                                        ?++ keyedPlayer
                                        ?++ keyedAim
                               ]
                   ]
                ++ floor 6
                ++ tiles 1
                ++ List.concat (List.repeat 6 <| tiles 1 ++ floor 7 ++ tiles 1)
                ++ tiles 9
        , div
            [ class
                (if model.state /= Respawnable then
                    "hidden"
                 else
                    ""
                )
            ]
            [ h2 [ class textClasses ] [ text "Tap or press space" ]
            , h2 [ class textClasses ] [ text <| "Highscore: " ++ toString model.highscore ]
            ]
        ]


positionStyle : Position -> Attribute msg
positionStyle pos =
    let
        ( x, y ) =
            Position.asTuple pos

        toPercent i =
            toString <| (i - 1) * 100
    in
    style [ ( "transform", "translate(" ++ toPercent x ++ "%, " ++ toPercent y ++ "%)" ) ]


aim : Bool -> Position -> Html msg
aim canMove pos =
    let
        ( color, transition ) =
            if canMove then
                ( "yellow", "transition-fast" )
            else
                ( "red", "transition" )
    in
    div [ class ("pin-t square absolute w-full z-30 " ++ transition), positionStyle pos ]
        [ div
            [ class <|
                "absolute pin square rounded-lg border-8 border-dotted border-"
                    ++ color
                    ++ "-dark"
            ]
            []
        ]


player : Position -> Html msg
player pos =
    div [ class "pin-t square absolute w-full z-20 transition", positionStyle pos ]
        [ div
            [ class <|
                "fadein absolute pin square rounded-full border-2 border-b-8 bg-green-light border-green-dark"
            ]
            []
        ]


enemy : Enemy -> ( String, Html msg )
enemy (( pos, _, key ) as enemy) =
    (,) (toString key) <|
        div
            [ classList
                [ ( "square absolute w-full z-10 transition", True )
                , ( "fadeout", moveEnemy enemy |> Maybe.isNothing )
                ]
            , positionStyle pos
            ]
        <|
            [ div
                [ class <|
                    "fadein absolute pin rounded-sm border-2 border-b-8 bg-red-light border-red-dark"
                ]
                []
            ]


tile : String -> Html msg
tile color =
    div [ class "square relative" ]
        [ div
            [ class <|
                "m-px absolute pin rounded-sm border-2 border-b-8 bg-"
                    ++ color
                    ++ "-light border-"
                    ++ color
                    ++ "-dark"
            ]
            []
        ]



-- Main


init : Model
init =
    { state = Respawnable
    , player = position 4 4
    , playerCanMove = True
    , aim = Left
    , score = 0
    , highscore = 0
    , enemies = []
    }


main : Program Int Model Msg
main =
    programWithFlags
        { init = \highscore -> ( { init | highscore = highscore }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { state } =
    let
        gameEvents =
            case state of
                Playing ->
                    [ Time.every (0.6 * Time.second) MoveEnemies
                    , Time.every (0.8 * Time.second) SpawnEnemy
                    , Time.every (0.7 * Time.second) MoveAim
                    ]

                CantRespawn ->
                    [ Time.every Time.second SetRespawnable ]

                Respawnable ->
                    []
    in
    Sub.batch
        (Keyboard.downs KeyDown
            :: Mouse.downs MouseDown
            :: gameEvents
        )


port saveHighscore : Int -> Cmd val
