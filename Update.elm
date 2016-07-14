module Draggable.Update exposing (..)

import Draggable.Model exposing (..)
import Draggable.Msg exposing (..)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ pos, drag } as model) =
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
      ( { model | position = Position (Result.withDefault position.x x) position.y }
      , Cmd.none
      )

    SetPositionY y ->
      ( { model | position = Position position.x (Result.withDefault position.y y) }
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
      model
      


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
            Position positionX position.x
          Y ->
            Position positionY position.y
          _ ->
            Position position.x position.y
