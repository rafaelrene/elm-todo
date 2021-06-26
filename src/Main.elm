module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Todo as Todo exposing (Msg(..), Todo, TodoStatus(..), view)



---- MODEL ----


type alias Model =
    { todoList : List Todo
    }


init : ( Model, Cmd Msg )
init =
    ( { todoList = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | TodoMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TodoMsg todoMsg ->
            let
                todo =
                    Todo.update todoMsg
            in
            ( { model | todoList = todo :: model.todoList }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        todoView =
            Todo.view "Something" |> Html.map (\msg -> TodoMsg msg)
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , todoView
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
