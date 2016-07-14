module Draggable.Subscriptions exposing (..)

import Draggable.Model exposing (..)


subscriptions : Model -> Sub Msg
subscriptions { drag } =
  case drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragMove, Mouse.ups DragEnd ]
