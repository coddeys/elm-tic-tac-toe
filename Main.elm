module Main exposing (..)

import Array.Hamt as Array exposing (..)
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)
import Matrix exposing (Matrix)
import Matrix.Extra
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Matrix (Maybe Player)
    , status : Status
    }


type Status
    = Turn Player
    | Win Player
    | Draw


type Player
    = X
    | O


playerToString : Player -> String
playerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"


init : ( Model, Cmd Msg )
init =
    { board = Matrix.repeat 3 3 Nothing
    , status = Turn X
    }
        ! [ Cmd.none ]


type Styles
    = Clean
    | Cell
    | UnitX
    | UnitO
    | UnitWrap
    | Board
    | Title
    | NextTurn


type alias Position =
    ( Int, Int )



-- UPDATE


type Msg
    = Select Int Int
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select x y ->
            let
                ( status, board ) =
                    updateGame x y model.status model.board
                        |> Debug.log "updateGame:"
            in
            { model
                | status = status
                , board = board
            }
                ! []

        Reset ->
            init


win : Matrix (Maybe Player) -> Maybe Player
win board =
    let
        rows =
            [ X, O ]
                |> List.map (checkRows board)

        columns =
            [ X, O ]
                |> List.map (checkColumns board)

        diagonals =
            [ X, O ]
                |> List.map (checkDiagonals board)
    in
    (rows ++ columns ++ diagonals)
        |> List.concat
        |> List.map (List.drop 2)
        |> List.concat
        |> List.head
        |> Debug.log "win: "


checkDiagonals : Matrix (Maybe Player) -> Player -> List (List Player)
checkDiagonals matrix player =
    [ unpackMaybeList (\( x, y ) -> Matrix.get x y matrix)
        [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , unpackMaybeList (\( x, y ) -> Matrix.get x y matrix)
        [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ) ]
    ]
        |> List.map Array.fromList
        |> List.map (check player)


checkColumns : Matrix (Maybe Player) -> Player -> List (List Player)
checkColumns matrix player =
    unpackMaybeList
        (\i -> Matrix.getColumn i matrix)
        (List.range 0 (Matrix.width matrix - 1))
        |> List.map (check player)


checkRows : Matrix (Maybe Player) -> Player -> List (List Player)
checkRows matrix player =
    unpackMaybeList
        (\i -> Matrix.getRow i matrix)
        (List.range 0 (Matrix.height matrix - 1))
        |> List.map (check player)


check : Player -> Array (Maybe Player) -> List Player
check player array =
    array
        |> Array.toList
        |> List.foldr (f player) []


f : Player -> Maybe Player -> List Player -> List Player
f player mPlayer players =
    case mPlayer of
        Nothing ->
            players

        Just next ->
            if next == player then
                player :: players
            else
                players


updateGame : Int -> Int -> Status -> Matrix (Maybe Player) -> ( Status, Matrix (Maybe Player) )
updateGame x y status board =
    case status of
        Win p ->
            ( Win p, board )

        Draw ->
            ( Draw, board )

        Turn player ->
            let
                newBoard =
                    updateBoard x y player board
            in
            if newBoard == board then
                ( status, newBoard )
            else if isMoveAvailabe newBoard then
                case Debug.log "win:" (win newBoard) of
                    Just pl ->
                        ( Win player, newBoard )

                    Nothing ->
                        ( Turn (swapPlayer player), newBoard )
            else
                ( Draw, newBoard )


isMoveAvailabe : Matrix (Maybe Player) -> Bool
isMoveAvailabe board =
    Matrix.filter (\x -> x == Nothing) board
        |> Array.isEmpty
        |> not


swapPlayer : Player -> Player
swapPlayer status =
    case status of
        X ->
            O

        O ->
            X


updateBoard : Int -> Int -> Player -> Matrix (Maybe Player) -> Matrix (Maybe Player)
updateBoard x y value board =
    Matrix.update x y (updatePlayer value) board


updatePlayer : Player -> Maybe Player -> Maybe Player
updatePlayer new current =
    case current of
        Nothing ->
            Just new

        Just _ ->
            current



-- VIEW


stylesheet =
    Style.styleSheet
        [ style Cell
            []
        , style UnitWrap
            [ Color.background white
            , Border.solid
            , Border.all 1
            , Style.cursor "pointer"
            ]
        , style UnitX
            [ Font.size 32
            , Color.text red
            ]
        , style UnitO
            [ Font.size 32
            , Color.text black
            ]
        , style Board
            [ Border.solid
            , Border.all 1
            ]
        , style Clean []
        , style Title [ Font.size 24 ]
        , style NextTurn
            [ Color.text red
            ]
        ]


view : Model -> Html Msg
view model =
    viewport stylesheet
        (mainContent Clean
            []
            (column Clean
                []
                [ titleView
                , el
                    Clean
                    [ center ]
                    (column Clean
                        [ spacing 10 ]
                        [ playersView model.status
                        , rows model.board
                        , resetButton
                        ]
                    )
                ]
            )
        )


titleView : Element Styles variation Msg
titleView =
    h1 Title [ center, padding 30 ] (text "TIC TAC TOE")


playersView : Status -> Element Styles variation Msg
playersView status =
    row Clean
        [ spread, padding 10 ]
        [ statusView status
        ]


resetButton : Element Styles variation Msg
resetButton =
    button Clean
        [ center
        , width (px 100)
        , padding 10
        , onClick Reset
        ]
        (text "Reset")


statusView : Status -> Element Styles variation Msg
statusView status =
    case status of
        Turn player ->
            paragraph Clean
                []
                [ el Clean [] (text "Next Turn: ")
                , el NextTurn [] (text (playerToString player))
                ]

        Win player ->
            paragraph Clean
                []
                [ el NextTurn [] (text "Win Player: ")
                , el NextTurn [] (text (playerToString player))
                ]

        Draw ->
            paragraph Clean
                []
                [ el Clean [] (text "Draw") ]


rows : Matrix (Maybe Player) -> Element Styles variation Msg
rows matrix =
    column Board
        [ spacing 5, padding 5 ]
        (List.indexedMap rowView
            (unpackMaybeList
                (\i -> Matrix.getRow i matrix)
                (List.range 0 (Matrix.height matrix - 1))
            )
        )


rowView : Int -> Array (Maybe Player) -> Element Styles variation Msg
rowView i statuses =
    row Clean [ spacing 5 ] (cells i statuses)


cells : Int -> Array (Maybe Player) -> List (Element Styles variation Msg)
cells x statuses =
    statuses
        |> toIndexedList
        |> List.map (\( y, a ) -> cell y x a)


cell : Int -> Int -> Maybe Player -> Element Styles variation Msg
cell x y player =
    column Cell
        []
        [ el UnitWrap
            [ padding 10
            , width (px 100)
            , height (px 100)
            , onClick (Select x y)
            ]
            (playerCell player)
        ]


playerCell : Maybe Player -> Element Styles variation Msg
playerCell mPlayer =
    case mPlayer of
        Nothing ->
            Element.empty

        Just X ->
            el UnitX
                [ center
                , verticalCenter
                ]
                (text "X")

        Just O ->
            el UnitO
                [ center
                , verticalCenter
                ]
                (text "O")



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none



-- Helper functions


unpackMaybeList : (a -> Maybe b) -> List a -> List b
unpackMaybeList fn ls =
    List.foldl
        (\item ls ->
            case fn item of
                Just it ->
                    ls ++ [ it ]

                Nothing ->
                    ls
        )
        []
        ls
