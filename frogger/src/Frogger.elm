module Frogger exposing (..)

import Html exposing (Html, program, div, text)


-- MSGS


type Msg
    = NoOp



-- MODEL


type alias Model =
    Int



-- INIT


init : ( Model, Cmd msg )
init =
    ( 0, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Hello Frogger" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
