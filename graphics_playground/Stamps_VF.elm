module Stamps exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html)
import Mouse
import Keyboard

type alias Position =
  (Int, Int)

type alias Model =
  { stamps : List Stamp
  , shift : Bool
  , ctrl : Bool
  }

type Shape
  = Pentagon
  | Circle
  
type alias Stamp =
  { position : Position
  , shape : Shape
  , color : Color
  }

type Msg
  = AddClick Position
  | HandleShift Bool
  | HandleCtrl Bool
  | Clear
  | NoOp

type Color
  = Red
  | Blue

init : Model
init =
  { stamps = []
  , shift = False
  , ctrl = False
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddClick pos ->
      let
        stampShape = case model.shift of
          True -> Pentagon
          False -> Circle

        stampColor = case model.ctrl of
          True -> Red
          False -> Blue

        newStamp = { position = pos, shape = stampShape, color = stampColor }

        listWithNewStamp = newStamp :: model.stamps
      in
        { model | stamps = List.take 5 listWithNewStamp } ! []

    HandleShift pressed ->
      { model | shift = pressed } ! []

    HandleCtrl pressed ->
      { model | ctrl = pressed } ! []

    Clear ->
      { model | stamps = [] } ! []

    NoOp -> 
      model ! []
  

-- drawStamp takes a position and return a graphics form
drawStamp : Stamp -> Form
drawStamp stamp =
  let
    (x, y) = stamp.position

    shape = case stamp.shape of 
      Pentagon -> ngon 5 50
      Circle -> circle 50

    color = case stamp.color of
      Blue -> blue
      Red -> red
  in
    shape
    |> filled color
    |> move (toFloat(x), toFloat(-1 * y))


-- We'll update the view to use our model
view : Model -> Html Msg
view model =
  let
    theGroup =
      -- Map a list of positions through our drawstamp function to get a list
      -- of forms, and put them in a group
      group (List.map drawStamp model.stamps)

    -- We'll move the group to the origin like we did in the previous examples
    originGroup =
      move (-400, 400) theGroup
  in
    -- Now make a collage containing the group
    collage 800 800
      [ originGroup ]
      |> Element.toHtml

clicks : List (Int, Int)
clicks =
  -- We'll just hardcode a list of positions
  [(0, 0), (100, 100), (200, 100)]

main =
  Html.program
    { init = (init, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks (\{x, y} -> AddClick (x, y))
    , Keyboard.downs mapKeyDown
    , Keyboard.ups mapKeyUp
    ]

mapKeyDown : Int -> Msg
mapKeyDown keyCode =
  case keyCode of
    16 -> HandleShift True
    17 -> HandleCtrl True
    67 -> Clear
    _  -> NoOp

mapKeyUp : Int -> Msg
mapKeyUp keyCode =
  case keyCode of
    16 -> HandleShift False
    17 -> HandleCtrl False
    _  -> NoOp












