port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { count : Int
    , totalIncrements : Int
    , totalDecrements : Int
    }


type Msg
    = Increment
    | Decrement
    | Set Model
    | NoOp


initialModel : Model
initialModel =
    { count = 0, totalIncrements = 0, totalDecrements = 0 }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newModel =
                    { model
                        | count = model.count + 1
                        , totalIncrements = model.totalIncrements + 1
                    }
            in
                ( newModel
                , Cmd.batch
                    [ increment ()
                    , storage newModel
                    ]
                )

        Decrement ->
            let
                newModel =
                    { model
                        | count = model.count - 1
                        , totalDecrements = model.totalDecrements + 1
                    }
            in
                ( newModel
                , storage newModel
                )

        Set storedModel ->
            ( storedModel
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        , h3 [] [ text ("Total increments: " ++ (toString (model.totalIncrements + 1))) ]
        , h3 [] [ text ("Total decrements: " ++ (toString model.totalDecrements)) ]
        ]



-- MAIN


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ jsMsgs mapJsMsg
        , storageInput Set
        ]



-- INPUT PORTS


port jsMsgs : (Int -> msg) -> Sub msg


mapJsMsg : Int -> Msg
mapJsMsg int =
    case int of
        1 ->
            Increment

        _ ->
            NoOp


port storageInput : (Model -> msg) -> Sub msg



-- OUTPUT PORTS


port increment : () -> Cmd msg


port storage : Model -> Cmd msg
