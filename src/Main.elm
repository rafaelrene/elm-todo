module Main exposing (..)

import Browser
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import Todo as Todo exposing (Model, Msg(..), Todo, TodoStatus(..), initialModel, view)



---- MODEL ----


type alias Model =
    { todo : Todo.Model }


init : ( Model, Cmd Msg )
init =
    ( { todo = Todo.initialModel }, Cmd.none )



---- UPDATE ----


type Msg
    = GotTodoMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodoMsg todoMsg ->
            let
                ( todo, command ) =
                    Todo.update todoMsg model.todo

                mainCommand =
                    Cmd.map GotTodoMsg command
            in
            ( { model | todo = todo }, mainCommand )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "wrapper" ]
        [ Html.h1 [ Attributes.class "heading" ] [ Html.text "todos" ]
        , Todo.view model.todo GotTodoMsg
        , Html.footer [ Attributes.class "footer" ]
            [ Html.p [ Attributes.class "footer__paragraph" ] [ Html.text "Double-click to edit todo" ]
            , Html.p [ Attributes.class "footer__paragraph" ] [ Html.text "Written by René Rafael" ]
            ]
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
