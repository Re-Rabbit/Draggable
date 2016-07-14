module Draggable exposing (..)

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Debug
import Json.Decode as Json exposing ((:=))
import String
import Html.App as Html


-- Model

type Axis
  = Both
  | X
  | Y


type alias Model =
  { pos  : Position
  , drag : Maybe Drag
  , axis : Axis
  , grid : Maybe Int
  }


type alias Drag =
  { start : Position
  , curr  : Position
  }


init : (Model, Cmd Msg)
init =
  ( { pos  = Position 0 0
    , drag = Nothing
    , axis = Both
    , grid = Nothing
    }
  , Cmd.none
  )


-- Update

type Msg
  = NoOp
  | DragStart Position
  | DragMove  Position
  | DragEnd   Position
  | SetPositionX String
  | SetPositionY String
  | SetAxis Axis
  | SetGrid (Maybe Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ pos, drag } as model) =
  case msg of
    DragStart pos' ->
      ( { model | drag = Just <| Drag pos' pos' }
      , Cmd.none
      )

    DragMove pos' ->
      ( { model | drag = Maybe.map (\{ start } -> Drag start pos') drag }
      , Cmd.none
      )

    DragEnd _ ->
      ( { model | pos = getPosition model, drag = Nothing }
      , Cmd.none
      )

    SetPositionX x ->
      ( { model | pos = Position (Result.withDefault pos.x <| String.toInt x) pos.y }
      , Cmd.none
      )

    SetPositionY y ->
      ( { model | pos = Position pos.x (Result.withDefault pos.y <| String.toInt y) }
      , Cmd.none
      )

    SetAxis a ->
      ( { model | axis = a }
      , Cmd.none
      )

    SetGrid g ->
      ( { model | grid = g }
      , Cmd.none
      )

    _ ->
      (model, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions { drag } =
  case drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragMove, Mouse.ups DragEnd ]


-- View

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

           


getPosition : Model -> Position
getPosition { pos, drag, axis, grid } =
  case drag of
    Nothing ->
      pos
    Just { start, curr } ->
      let
        relative a =
          (a curr) - (a start)
        relativeX =
          relative .x
        relativeY =
          relative .y

        stepGrow b s =
          b // s * s

        pos' =
          case grid of
            Nothing ->
              ( pos.x + relativeX
              , pos.y + relativeY
              )
            Just g ->
              ( pos.x + stepGrow relativeX g
              , pos.y + stepGrow relativeY g
              )

        (posx, posy) =
          pos'
      in
        case axis of
          X ->
            Position posx pos.y
          Y ->
            Position pos.x posy
          _ ->
            Position posx posy


-- MAIN

main : Program Never
main =
  Html.program
    { init   = init
    , update = update
    , view   = view
    , subscriptions = subscriptions
    }
