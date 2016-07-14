module Draggable exposing (..)

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Debug
import Json.Decode as Json exposing ((:=))
import String
import Html.App as Html


import Draggable.Model         exposing (..)
import Draggable.Msg           exposing (..)
import Draggable.Init          exposing (..)
import Draggable.Update        exposing (..)
import Draggable.Subscriptions exposing (..)


type alias Model =
  { draggable : Draggable.Model
  }


initModel : Model
initModel =
  Model Draggable.Model.initModel


init : (Model, Cmd Msg)
init =
  let
    model =
      initModel
  in
    (model , Cmd.none)      

type Msg
  = NoOp
  | DraggableMsg Draggable.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DraggableMsg msg' ->
      let
        (m, fx) =
          Draggable.update msg' model.draggable
      in
        ( { model | draggable = m }
        , fx
        )
    
    _ ->
      (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map DraggableMsg Draggable.Subscriptions.subscriptions
    ]


view : Model -> Html Msg
view model =
  div []
    [ div
        [ class "cursor"
        , cursorStyle model
        , on "mousedown" (Json.map DragStart Mouse.position)
        ] [ text "不如 D" ]

    , label []
        [ text "设置x："
        , input
            [ onInput SetPositionX
            , value <| toString <| .x <| getPosition model
            ] []
        ]
    , label []
        [ text "设置Y："
        , input
            [ onInput SetPositionY
            , value <| toString <| .y <| getPosition model
            ] []
        ]
    , div []
        [ text "选择坐标："
        , text "Both"
        , input
            [ type' "radio"
            , onCheck (\_ -> SetAxis Both)
            , checked <| (toString model.axis) == "Both"
            ] []
        , text "X"
        , input
            [ type' "radio"
            , onCheck (\_ -> SetAxis X)
            , checked <| (toString model.axis) == "X"
            ] []
        , text "Y"
        , input
            [ type' "radio"
            , onCheck (\_ -> SetAxis Y)
            , checked <| (toString model.axis) == "Y"
            ] []
            
        ]
      , div []
        [ text "设置step："
        , text "Nope"
        , input
            [ type' "radio"
            , onCheck (\_ -> SetGrid Nothing)
            , checked <| (toString model.grid) == "Nothing"
            ] []
        , text "Yep"
        , input
            [ type' "radio"
            , onCheck (\_ -> SetGrid <| Just 1)
            , checked <| (toString model.grid) /= "Nothing"
            ] []
        , input
            [ onInput (\s -> SetGrid <| Result.toMaybe <| String.toInt s)
            , value <| toString <| Maybe.withDefault 1 model.grid
            ] []
        ]
    ]
    
    
(=>) = (,)

cursorStyle : Model -> Attribute msg
cursorStyle ({ pos, drag } as model) =
  let
    {x, y} =
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


intToPxStr : Int -> String
intToPxStr n =
  toString n ++ "px"


-- MAIN

main : Program Never
main =
  Html.program
    { init   = init
    , update = update
    , view   = view
    , subscriptions = subscriptions
    }
