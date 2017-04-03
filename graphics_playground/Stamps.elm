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
  }

type Shape
  = Pentagon
  | Circle
  
type alias Stamp =
  { position : Position
  , shape : Shape
  }

type Msg
  = AddClick Position
  | HandleShift Bool
  | NoOp

init : Model
init =
  { stamps = []
  , shift = False
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddClick pos ->
      let
        stampShape = case model.shift of
          True -> Pentagon
          False -> Circle
        newStamp = { position = pos, shape = stampShape }
      in
        { model | stamps = newStamp :: model.stamps } ! []

    HandleShift pressed ->
      { model | shift = pressed } ! []

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
  in
    shape
    |> filled red
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
    _  -> NoOp

mapKeyUp : Int -> Msg
mapKeyUp keyCode =
  case keyCode of
    16 -> HandleShift False
    _  -> NoOp
