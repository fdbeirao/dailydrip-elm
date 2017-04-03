port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, autofocus, type_, checked, value, href)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode
import List.Extra exposing (maximumBy)


-- MODEL


type alias TodoId =
    Int


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : TodoId
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    }


emptyTodo : Todo
emptyTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


type Msg
    = SetTodoTitle String
    | Add
    | SetCompletion Bool Todo
    | Delete Todo
    | DeleteCompletedTodos
    | Filter FilterState
    | SetModel Model
    | NoOp


initialModel : Model
initialModel =
    let
        initialTodos =
            [ { title = "Milk and cookies"
              , completed = True
              , editing = False
              , identifier = 1
              }
            ]

        nextTodoId =
            nextIdentifier initialTodos
    in
        { todos = initialTodos
        , todo = todoWithIdentifier emptyTodo nextTodoId
        , filter = All
        }


nextIdentifier : List Todo -> TodoId
nextIdentifier todos =
    case maximumBy (\todo -> todo.identifier) todos of
        Just maxIdTodo ->
            maxIdTodo.identifier + 1

        Nothing ->
            1



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTodoTitle newTodoTitle ->
            let
                newModel =
                    { model | todo = (todoWithTitle model.todo newTodoTitle) }
            in
                ( newModel, sendToStorage newModel )

        Add ->
            let
                newTodosList =
                    model.todo :: model.todos

                nextTodoIdentifier =
                    nextIdentifier newTodosList

                newModel =
                    { model
                        | todos = newTodosList
                        , todo = todoWithIdentifier emptyTodo nextTodoIdentifier
                    }
            in
                ( newModel, sendToStorage newModel )

        SetCompletion complete todo ->
            let
                markTodoAsComplete thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        todoWithCompletion todo complete
                    else
                        thisTodo

                newModel =
                    { model | todos = List.map markTodoAsComplete model.todos }
            in
                ( newModel, sendToStorage newModel )

        Delete todoToDelete ->
            let
                newModel =
                    { model | todos = List.filter (\todo -> todo.identifier /= todoToDelete.identifier) model.todos }
            in
                ( newModel, sendToStorage newModel )

        DeleteCompletedTodos ->
            let
                newModel =
                    { model | todos = List.filter (\todo -> not todo.completed) model.todos }
            in
                ( newModel, sendToStorage newModel )

        Filter filterState ->
            let
                newModel =
                    { model | filter = filterState }
            in
                ( newModel, sendToStorage newModel )

        SetModel restoredModel ->
            ( restoredModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


sendToStorage : Model -> Cmd Msg
sendToStorage model =
    encodeModelAsJson model |> storageOutput


todoWithTitle : Todo -> String -> Todo
todoWithTitle todo newTitle =
    { todo | title = newTitle }


todoWithIdentifier : Todo -> TodoId -> Todo
todoWithIdentifier todo newIdentifier =
    { todo | identifier = newIdentifier }


todoWithCompletion : Todo -> Bool -> Todo
todoWithCompletion todo completion =
    { todo | completed = completion }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , value model.todo.title
                    , autofocus True
                    , onEnter Add
                    , onInput SetTodoTitle
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ]
                    (List.map todoView (filteredTodos model))
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong []
                        [ text (toString (remainingTodos model.todos))
                        , text " items left"
                        ]
                    ]
                , ul [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button
                    [ class "clear-completed"
                    , onClick DeleteCompletedTodos
                    ]
                    [ text "Clear completed" ]
                ]
            ]
        ]


remainingTodos : List Todo -> Int
remainingTodos todos =
    List.length (List.filter (\todo -> not todo.completed) todos)


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


filteredTodos : Model -> List Todo
filteredTodos model =
    case model.filter of
        All ->
            model.todos

        Active ->
            List.filter (\todo -> not todo.completed) model.todos

        Completed ->
            List.filter (\todo -> todo.completed) model.todos


todoView : Todo -> Html Msg
todoView todo =
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onCheck (\completion -> SetCompletion completion todo)
                ]
                []
            , label [] [ text todo.title ]
            , button [ class "destroy", onClick (Delete todo) ] []
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Decode.andThen isEnter)



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
    storageInput mapStorageInput


encodeModelAsJson : Model -> Json.Encode.Value
encodeModelAsJson model =
    Json.Encode.object
        [ ( "todos", Json.Encode.list (List.map encodeTodoAsJson model.todos) )
        , ( "todo", encodeTodoAsJson model.todo )
        , ( "filter", encodeFilterStateAsJson model.filter )
        ]


encodeTodoAsJson : Todo -> Json.Encode.Value
encodeTodoAsJson todo =
    Json.Encode.object
        [ ( "title", Json.Encode.string todo.title )
        , ( "completed", Json.Encode.bool todo.completed )
        , ( "editing", Json.Encode.bool todo.editing )
        , ( "identifier", Json.Encode.int todo.identifier )
        ]


encodeFilterStateAsJson : FilterState -> Json.Encode.Value
encodeFilterStateAsJson filterState =
    Json.Encode.string (toString filterState)



-- INPUT PORTS


port storageInput : (Decode.Value -> msg) -> Sub msg



-- OUTPUT PORTS


port storageOutput : Json.Encode.Value -> Cmd msg



-- we're going to be decoding JSON and producing a message.  The JSON could be
-- malformed or otherwise inappropriate, so `Decode` functions will return
-- a `Result` type that can be either `Ok Model`  or `Err String`.
-- If we get an error, we send a NoOp, otherwise we send a SetModel message


mapStorageInput : Decode.Value -> Msg
mapStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            SetModel model

        Err errorMessage ->
            let
                -- If there's an error decoding it, we can show it in the
                -- console
                _ =
                    Debug.log "Error in mapStorageInput:" errorMessage
            in
                NoOp



-- The actual Decode function takes the modelJson and returns a result.  Here we
-- invoke the `decodeValue` function, where the first argument is a Decoder and
-- the second is the JSON string we're decoding


decodeModel : Decode.Value -> Result String Model
decodeModel modelJson =
    Decode.decodeValue modelDecoder modelJson


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "todos" (Decode.list todoDecoder)
        |> required "todo" todoDecoder
        |> required "filter" (Decode.string |> Decode.map filterStateDecoder)


todoDecoder : Decode.Decoder Todo
todoDecoder =
    decode Todo
        |> required "title" Decode.string
        |> required "completed" Decode.bool
        |> required "editing" Decode.bool
        |> required "identifier" Decode.int



-- Now all that's left is the filterStateDecoder.  For this we'll just use
-- `Json.Decode.map` to map through a function that turns a String into a
-- FilterState.


filterStateDecoder : String -> FilterState
filterStateDecoder string =
    case string of
        "All" ->
            All

        "Active" ->
            Active

        "Completed" ->
            Completed

        _ ->
            let
                _ =
                    Debug.log "filterStateDecoder" <|
                        "Couldn't decode value "
                            ++ string
                            ++ " so defaulting to All."
            in
                All
