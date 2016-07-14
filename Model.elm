module Draggable.Model exposing (..)

import Mouse exposing (Position)

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
  Model (Position 0 0) Nothing Both Nothing
