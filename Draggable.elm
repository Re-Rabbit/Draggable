module Draggable.Draggable exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Json.Decode as Json



-- MODEL

type Axis
  = Both
  | X
  | Y

type alias Scope =
  { minX : Maybe Int
  , maxX : Maybe Int
  , minY : Maybe Int
  , maxY : Maybe Int
  }

type alias Drag =
  { start : Position
  , curr  : Position
  }

type alias Model =
  { position : Position
  , drag     : Maybe Drag
  , axis     : Axis
  , grid     : Maybe Int
  , scope    : Scope
  }


initScope : Scope
initScope =
  { minX = Nothing
  , maxX = Nothing
  , minY = Nothing
  , maxY = Nothing
  }

initModel : Model
initModel =
  { position = Position 0 0
  , drag     = Nothing
  , axis     = Both
  , grid     = Nothing
  , scope    = initScope
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
getPosition { position, drag, axis, grid, scope } =
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
        scopeVal =
          valueInScope scope
      in
        case axis of
          X ->
            Position (scopeVal X positionX) position.y
          Y ->
            Position position.x (scopeVal Y positionY)
          _ ->
            Position (scopeVal X positionX) (scopeVal Y positionY)


valueInScope : Scope -> Axis -> Int -> Int
valueInScope { minX, minY, maxX, maxY } axis val =
  let
    defaultVal cmp =
      Maybe.withDefault val cmp
  in
    case axis of
      X ->
        clamp (defaultVal minX) (defaultVal maxX) val
      Y ->
        clamp (defaultVal minY) (defaultVal maxY) val
      _ ->
        val


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


view : List (Attribute Msg) -> Model -> Html Msg
view attrs model =
    let
      draggableAttrs =
        [ on "mousedown" (Json.map DragStart Mouse.position)
        , cursorStyle model
        ]
      view' =
        div (attrs ++ draggableAttrs) []
    in
      view'


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
      [ "transform" => ("translate3d(" ++ (intToPxStr x) ++ "," ++ (intToPxStr y) ++ ",0)")
      , "cursor" => "pointer"
      ]
