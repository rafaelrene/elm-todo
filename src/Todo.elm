module Todo exposing (Model, Msg(..), Todo, TodoStatus(..), initialModel, update, view)

import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Random as Random exposing (initialSeed, step)
import UUID exposing (Seeds, UUID)


type TodoStatus
    = OPEN
    | EDITING String
    | CLOSED


type alias Todo =
    { id : String
    , label : String
    , status : TodoStatus
    }


type alias Model =
    { todoList : List Todo
    , seed : Random.Seed
    , addTodoText : String
    }


type Msg
    = UpdateAddTodoText String
    | CreateTodo Int


initialModel : Model
initialModel =
    { todoList = [], seed = Random.initialSeed 0, addTodoText = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAddTodoText currentAddTodoText ->
            { model | addTodoText = currentAddTodoText }

        -- ENTER KEY CODE
        CreateTodo 13 ->
            let
                ( id, newSeed ) =
                    Random.step UUID.generator model.seed

                idAsString =
                    UUID.toString id

                todoList =
                    if String.length model.addTodoText > 0 then
                        Todo idAsString model.addTodoText OPEN :: model.todoList

                    else
                        model.todoList
            in
            { model | todoList = todoList, seed = newSeed, addTodoText = "" }

        CreateTodo _ ->
            model



---- VIEWS ----


view : Model -> (Msg -> mainMsg) -> Html mainMsg
view model mainMsg =
    Html.main_ [ Attributes.class "todo-list-wrapper" ] [ viewAddTodo model.addTodoText, viewTodoList model.todoList ]
        |> Html.map (\msg -> mainMsg msg)


viewAddTodo addTodoText =
    Html.section [ Attributes.class "add-todo" ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.class "add-todo__checkbox"
            ]
            []
        , Html.input
            [ Attributes.type_ "text"
            , Attributes.class "add-todo__input"
            , Attributes.placeholder "What needs to be done?"
            , Attributes.autofocus True
            , Attributes.value addTodoText
            , Events.onInput UpdateAddTodoText
            , onKeyDown CreateTodo
            ]
            []
        ]


viewTodoList todoList =
    List.map viewTodo todoList |> Html.ul [ Attributes.class "todo-list" ]


viewTodo todo =
    Html.li
        [ Attributes.class "todo-list-item"
        , Attributes.attribute "data-id" todo.id
        ]
        [ Html.input [ Attributes.type_ "checkbox", Attributes.class "todo-list-item__checkbox" ] []
        , Html.div [ Attributes.class "todo-list-item__label" ] [ Html.text todo.label ]
        , Html.button [ Attributes.class "todo-list-item__destroy" ] [ Html.text "X" ]
        ]



---- CUSTOM EVENTS ----


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    Json.map tagger Events.keyCode |> Events.on "keydown"
