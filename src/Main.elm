module Main exposing (..)

{-| Classic Snake game implemented in Elm, using elm-ui to display
its simple graphics.
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (href, rel, style)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as L
import Time
import Json.Decode as Decode
import Random


type alias Position =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Position


type alias Food =
    List Position


type TileType
    = TileEmpty
    | TileSnakeBody
    | TileFood


{-| All possible statuses of the game.
-}
type GameStatus
    = Playing
    | Lost String
    | NotStarted
    | Paused


{-| Simple tuple that indicates horizontal and vertical direction
as one of -1, 0, 1
-}
type alias Direction =
    ( Int, Int )


{-| Directions for the snake. Since we are dealing with a grid
and not pixel coordinates, it is easier to express the directions
as tuple for horizontal and vertical direction. Then we can apply
this to the snake update logic.
-}
type alias Directions =
    { right : Direction
    , left : Direction
    , up : Direction
    , down : Direction
    }


{-| Config holds defaults for the game.
-}
type alias Config =
    { fieldWidth : Int
    , fieldHeight : Int
    , tileSize : Int
    , foodCount : Int
    , initialSnake : Snake
    , initialUpdateRate : Float
    }


{-| Set some reasonable defaults for the game.
-}
config : Config
config =
    { fieldWidth = 30
    , fieldHeight = 30
    , tileSize = 15
    , foodCount = 5
    , initialSnake = [ Position 12 15, Position 11 15, Position 10 15 ]
    , initialUpdateRate = 200
    }


directions : Directions
directions =
    { right = ( 1, 0 )
    , left = ( -1, 0 )
    , up = ( 0, -1 )
    , down = ( 0, 1 )
    }


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias PlayField =
    { width : Int
    , height : Int
    , data : List TileType
    }


type alias Model =
    { snake : Snake
    , food : Food
    , field : PlayField
    , direction : Direction
    , gameStatus : GameStatus
    , updateRate : Float
    , score : Int
    }


type CollisionTestResult
    = NoCollision
    | AteFood
    | BitMyself
    | HitTheWall


type Msg
    = Move Direction
    | Tick Time.Posix
    | RandomPositions (List Position)
    | IgnoreKey
    | ChangeUpdateRate
    | TogglePause
    | Start


newPlayField : Int -> Int -> PlayField
newPlayField w h =
    PlayField w h (List.repeat (w * h) TileEmpty)


newModel : GameStatus -> Model
newModel gameStatus =
    { snake = config.initialSnake
    , food = []
    , field = newPlayField config.fieldWidth config.fieldHeight
    , direction = directions.right
    , gameStatus = gameStatus
    , updateRate = config.initialUpdateRate
    , score = 0
    }


{-| Create a list of Positions
-}
randomPositions : Int -> Cmd Msg
randomPositions count =
    Random.generate RandomPositions <|
        Random.list count positionGenerator


{-| Create Position with random coordinates.
-}
positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2
        (\x y -> Position x y)
        (Random.int 0 <| config.fieldWidth - 1)
        (Random.int 0 <| config.fieldHeight - 1)


init : () -> ( Model, Cmd Msg )
init _ =
    ( newModel NotStarted
    , randomPositions config.foodCount
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomPositions positions ->
            ( { model
                | food = model.food ++ diffList positions model.snake
              }
            , Cmd.none
            )

        Move direction ->
            if model.gameStatus == Playing then
                ( { model | direction = direction }, Cmd.none )
            else
                ( model, Cmd.none )

        Tick posixTime ->
            let
                ( newSnake, removedPart ) =
                    updateSnake model.direction model.snake

                ( collisionTestResult, snakeHead ) =
                    evalCollision newSnake model.food

                ( newFood, newScore ) =
                    case snakeHead of
                        Just h ->
                            if collisionTestResult == AteFood then
                                ( List.filter (\x -> x /= h) model.food
                                , model.score + 1
                                )
                            else
                                ( model.food, model.score )

                        Nothing ->
                            ( model.food, model.score )
            in
                if model.gameStatus == Playing then
                    ( { model
                        | snake =
                            case collisionTestResult of
                                AteFood ->
                                    newSnake ++ removedPart

                                HitTheWall ->
                                    model.snake

                                _ ->
                                    newSnake
                        , gameStatus = collisionToGameStatus collisionTestResult
                        , food = newFood
                        , score = newScore
                        , updateRate =
                            if model.updateRate > 50 && collisionTestResult == AteFood then
                                model.updateRate - 5
                            else
                                model.updateRate
                      }
                    , if List.length model.food < 2 then
                        randomPositions 5
                      else
                        Cmd.none
                    )
                else
                    ( model, Cmd.none )

        ChangeUpdateRate ->
            ( { model
                | updateRate = model.updateRate - 20
              }
            , Cmd.none
            )

        TogglePause ->
            if model.gameStatus == Paused then
                ( { model | gameStatus = Playing }, Cmd.none )
            else if model.gameStatus == Playing then
                ( { model | gameStatus = Paused }, Cmd.none )
            else
                ( model, Cmd.none )

        Start ->
            if model.gameStatus == NotStarted then
                ( { model | gameStatus = Playing }, Cmd.none )
            else if isLost model.gameStatus then
                ( newModel Playing, randomPositions config.foodCount )
            else
                ( model, Cmd.none )

        IgnoreKey ->
            ( model, Cmd.none )


diffList : List a -> List a -> List a
diffList aList bList =
    List.filter (\c -> not <| List.member c bList) aList


updateSnake : Direction -> Snake -> ( Snake, List Position )
updateSnake ( xDir, yDir ) snake =
    let
        h =
            Maybe.withDefault (Position -1 -1) (List.head snake)

        newHead =
            Position (h.x + xDir) (h.y + yDir)

        newBody =
            List.take ((List.length snake) - 1) snake

        removedPart =
            List.drop ((List.length snake) - 1) snake
    in
        ( [ newHead ] ++ newBody, removedPart )


{-| Evaluate possible collision and return collision type with position
of where the collision occurred.
-}
evalCollision : Snake -> Food -> ( CollisionTestResult, Maybe Position )
evalCollision snake food =
    case snake of
        h :: t ->
            if List.member h t then
                ( BitMyself, Just h )
            else if h.x < 0 || h.y < 0 || h.x == config.fieldWidth || h.y == config.fieldHeight then
                ( HitTheWall, Just h )
            else if List.member h food then
                ( AteFood, Just h )
            else
                ( NoCollision, Nothing )

        _ ->
            ( NoCollision, Nothing )


{-| Certain type of collision results in game status change.
-}
collisionToGameStatus : CollisionTestResult -> GameStatus
collisionToGameStatus collisionTestResult =
    case collisionTestResult of
        NoCollision ->
            Playing

        AteFood ->
            Playing

        BitMyself ->
            Lost "YOU BIT YOURSELF !"

        HitTheWall ->
            Lost "YOU HIT THE WALL !"


{-| Subscriptions to keyboard events and timer
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every model.updateRate Tick
        , Browser.Events.onKeyDown keyDecoder
        ]


isLost : GameStatus -> Bool
isLost gameStatus =
    case gameStatus of
        Lost _ ->
            True

        _ ->
            False


{-| Decoder for the pressed key
-}
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMessage (Decode.field "key" Decode.string)


{-| Classify pressed key and fire proper message.
-}
keyToMessage : String -> Msg
keyToMessage string =
    case String.uncons string of
        Just ( char, "" ) ->
            case String.toLower <| String.fromChar char of
                "q" ->
                    ChangeUpdateRate

                "p" ->
                    TogglePause

                " " ->
                    Start

                _ ->
                    IgnoreKey

        _ ->
            case string of
                "ArrowRight" ->
                    Move directions.right

                "ArrowLeft" ->
                    Move directions.left

                "ArrowUp" ->
                    Move directions.up

                "ArrowDown" ->
                    Move directions.down

                _ ->
                    IgnoreKey


{-| Browser.document requires the view to return Document type. So we
define the type alias for Document here and use it as a return type from
the view function.
-}
type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


{-| Entry point for the view. Here we want to set the main style of the game
page like the frame filling the entire page, background color and fancy font.
-}
view : Model -> Document Msg
view model =
    { title = "Snake game with elm-ui"
    , body =
        [ Element.layout
            ([ Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Russo+One"
                    , name = "Russo One"
                    }
                , Font.sansSerif
                ]
             , Font.color gameColors.body
             , Background.color gameColors.frame1
             ]
            )
          <|
            el
                ([ centerX ]
                    ++ (styleGameFrame gameColors.frame1 gameColors.frame1 False)
                )
                (if model.gameStatus == NotStarted then
                    viewStart
                 else
                    L.lazy viewGame model
                )
        ]
    }


{-| Show Start screen with game instructions.[]
-}
viewStart : Element Msg
viewStart =
    el (styleGameFrame gameColors.frame2 gameColors.black True) <|
        column [ centerX, centerY, spacing 40, width <| px 300 ]
            [ viewTitle
            , column [ spacing 15 ]
                [ paragraph [ Font.center ]
                    [ text "USE "
                    , el [ Font.color gameColors.yellow ] (text "ARROW KEYS")
                    , text " TO CHANGE SNAKE DIRECTION"
                    ]
                , paragraph [ Font.center ]
                    [ text "PRESS "
                    , el [ Font.color gameColors.yellow ] (text "P")
                    , text " TO PAUSE THE GAME"
                    ]
                , paragraph [ Font.center ] [ text "YOU WILL DIE IF YOU BITE YOURSELF OR HIT THE WALL." ]
                , paragraph [ Font.center ] [ text "THE MORE YOU EAT, THE LONGER YOU GET AND THE FASTER YOU GO." ]
                , paragraph [ Font.center ] [ text "THE FOOD IS RANDOMLY PLACED ON THE FIELD IN BATCHES." ]
                ]
            , paragraph [ Font.center, Font.color gameColors.body ]
                [ text "PRESS ", el [ Font.color gameColors.yellow ] (text "SPACEBAR"), text " TO START" ]
            ]


{-| Game colors. Lets put some meaning behind numbers.
-}
gameColors =
    { black = rgb255 0 0 0
    , yellow = rgb255 255 255 0
    , body = rgb255 255 255 255
    , snake = rgb255 0 220 0
    , tile = rgb255 50 20 20
    , food = rgb255 180 0 0
    , wall = rgb255 80 50 50
    , frame1 = rgb255 60 110 60
    , frame2 = rgb255 80 180 80
    , title = rgb255 0 220 0
    }


{-| Main view of the game
-}
viewGame : Model -> Element Msg
viewGame model =
    el (styleGameFrame gameColors.frame2 gameColors.black True) <|
        column
            ([ centerX, centerY, spacing 30 ]
                ++ (viewMessage model.gameStatus)
            )
        <|
            [ viewTitle
            , el [] <| viewField model
            , viewScore model.score
            ]


{-| View for both static and action field.
-}
viewField : Model -> Element Msg
viewField model =
    let
        width_ =
            model.field.width * config.tileSize

        tileCount =
            List.length model.field.data
    in
        el
            [ Border.color gameColors.wall
            , Border.width 15
            , Border.rounded 8
            ]
            (el (viewActionField model) <|
                L.lazy3 viewStaticField TileEmpty tileCount width_
            )


{-| Static field is just a playground UI that does not change. It serves
as a visual cue of the background.
-}
viewStaticField : TileType -> Int -> Int -> Element Msg
viewStaticField tileType count width_ =
    wrappedRow
        [ width <| px width_ ]
    <|
        List.map (\_ -> viewTile tileType Nothing) (List.repeat count 0)


{-| Overlay in front of the static field.
Here is where all the UI action happens.
-}
viewActionField : Model -> List (Element.Attribute Msg)
viewActionField model =
    let
        element =
            \a tileType -> Element.inFront <| viewTile tileType <| Just a
    in
        List.map (\x -> element x TileSnakeBody) model.snake
            ++ List.map (\x -> element x TileFood) model.food


{-| Title of the game
-}
viewTitle : Element Msg
viewTitle =
    el
        [ Font.color gameColors.title
        , Font.size 50
        , Font.bold
        , centerX
        ]
        (text "SNAKE !")


viewScore : Int -> Element Msg
viewScore score =
    row [ centerX ]
        [ el [ width <| px 100 ] (text "SCORE : ")
        , el [ width <| px 20 ] (text <| String.fromInt score)
        ]


viewMessage : GameStatus -> List (Element.Attribute Msg)
viewMessage status =
    case status of
        Lost string ->
            [ Element.inFront
                (el
                    [ centerX
                    , centerY
                    , padding 20
                    , Border.solid
                    , Border.color gameColors.body
                    , Border.width 1
                    , Border.rounded 5
                    , Background.color gameColors.black
                    ]
                 <|
                    column
                        [ spacing 10 ]
                        [ paragraph [ Font.center ] [ text string ]
                        , paragraph [ Font.center ] [ text "PRESS SPACEBAR TO RESTART THE GAME" ]
                        ]
                )
            ]

        _ ->
            []


{-| Show tile in specified position. When rendering static field, no position
is provided and the tile is displayed as a regular inline element.
-}
viewTile : TileType -> Maybe Position -> Element Msg
viewTile tileType position =
    el
        ((styleTile tileType)
            ++ case position of
                Just p ->
                    [ moveRight <| toFloat (p.x * config.tileSize)
                    , moveDown <| toFloat (p.y * config.tileSize)
                    ]

                Nothing ->
                    []
        )
        none


{-| Style of the tile, based on tile type
-}
styleTile : TileType -> List (Element.Attribute Msg)
styleTile tileContent =
    let
        tileColor =
            case tileContent of
                TileEmpty ->
                    gameColors.tile

                TileSnakeBody ->
                    gameColors.snake

                TileFood ->
                    gameColors.food
    in
        [ width <| px config.tileSize
        , height <| px config.tileSize
        , Background.color tileColor
        , Border.color gameColors.black
        , Border.width 1
        , Border.solid
        , Border.rounded 3
        ]


{-| Helper function to make fancy border inside the game browser window.
-}
styleGameFrame : Element.Color -> Element.Color -> Bool -> List (Element.Attribute Msg)
styleGameFrame brcolor bgcolor rounded_ =
    [ width (Element.fill |> Element.maximum 700 |> Element.minimum 500)
    , height fill
    , Background.color bgcolor
    , Border.color brcolor
    , Border.width 10
    ]
        ++ if rounded_ then
            [ Border.rounded 10 ]
           else
            []
