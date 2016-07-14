module Draggable.Msg exposing (..)

import Mouse exposing (Position)

type Msg =
  = NoOp
  | DragStart Position
  | DragMove Position
  | DragEnd Position
  | SetPositionX Int
  | SetPositionY Int
  | SetAxis Axis
  | SetGrid (Maybe Int)
