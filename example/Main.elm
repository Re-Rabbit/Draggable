module Draggable.Example.Main exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Mouse exposing (Position)
import Json.Decode as Json

import Draggable.Draggable as Draggable


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Axis
  = Both
  | X
  | Y

type alias Drag =
  { start : Position
  , curr  : Position
  }

type alias Model =
  { position : Position
  , drag     : Maybe Drag
  , axis     : Axis
  , grid     : Maybe Int
  }

initModel : Model
initModel =
  { position = Position 0 0
  , drag     = Nothing
  , axis     = Both
  , grid     = Nothing
  }

init : (Model, Cmd Msg)
init =
  (initModel, Cmd.none)


-- UPDATE

type Msg
  = NoOp
  | DragStart Position
  | DragMove Position
  | DragEnd Position
  | SetPositionX Int
  | SetPositionY Int
  | SetAxis Axis
  | SetGrid (Maybe Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ position, drag } as model) =
  case msg of
    DragStart position ->
      ( { model | drag = Just <| Drag position position }
      , Cmd.none
      )

    DragMove position ->
      ( { model | drag = Maybe.map (\{ start } -> Drag start position) drag }
      , Cmd.none
      )

    DragEnd _ ->
      ( { model |
          position = getPosition model
        , drag = Nothing
        }
      , Cmd.none
      )

    SetPositionX x ->
      ( { model | position = Position x position.y }
      , Cmd.none
      )

    SetPositionY y ->
      ( { model | position = Position position.x y }
      , Cmd.none
      )

    SetAxis axis ->
      ( { model | axis = axis }
      , Cmd.none
      )

    SetGrid grid ->
      ( { model | grid = grid }
      , Cmd.none
      )

    _ ->
      (model, Cmd.none)



getPosition : Model -> Position
getPosition { position, drag, axis, grid } =
  case drag of
    Nothing ->
      position
    Just { start, curr } ->
      let
        relative f =
          (f curr) - (f start)

        relativeX =
          relative .x
        relativeY =
          relative .y

        stepGrow b s =
          b // s * s

        (positionX, positionY) =
          case grid of
            Nothing ->
              ( position.x + relativeX
              , position.y + relativeY
              )
            Just grid' ->
              ( position.x + stepGrow relativeX grid'
              , position.y + stepGrow relativeY grid'
              )
      in
        case axis of
          X ->
            Position positionX position.y
          Y ->
            Position position.x positionY
          _ ->
            Position positionX positionY


-- SUBSCRITIONS

subscriptions : Model -> Sub Msg
subscriptions { drag } =
  case drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch
        [ Mouse.moves DragMove
        , Mouse.ups DragEnd
        ]


view : Model -> Html Msg
view model =
    div [ on "mousedown" (Json.map DragStart Mouse.position)
        , cursorStyle model
        ]
        [ text "test1" ]


(=>) = (,)

intToPxStr : Int -> String
intToPxStr n =
    toString n ++ "px"

cursorStyle : Model -> Attribute msg
cursorStyle ({ position, drag } as model) =
    let
      { x, y } =
        getPosition model
    in
        style
            [ "width" => "140px"
            , "height" => "140px"
            , "borderRadius" => "6px"
            , "backgroundColor" => "rgb(76, 103, 194)"
            , "transform" => ("translate3d(" ++ (intToPxStr x) ++ "," ++ (intToPxStr y) ++ ",0)")
            , "cursor" => "pointer"
            , "color" => "white"
            , "textAlign" => "center"
            , "lineHeight" => "140px"
            ]
