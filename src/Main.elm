module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Todo as Todo exposing (Model, Msg(..), Todo, TodoStatus(..), initialModel, view)



---- MODEL ----


type alias Model =
    { todoList : Todo.Model }


init : ( Model, Cmd Msg )
init =
    ( { todoList = Todo.initialModel }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | GotTodoMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTodoMsg todoMsg ->
            let
                todoList =
                    Todo.update todoMsg model.todoList
            in
            ( { model | todoList = todoList }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , Todo.view model.todoList GotTodoMsg
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
