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
    | UpdateTodoStatus String Bool
    | UpdateAllTodoStatuses Bool


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

        UpdateTodoStatus idTodo isClosed ->
            let
                currentStatus =
                    if isClosed == True then
                        CLOSED

                    else
                        OPEN

                todoList =
                    List.map
                        (\todo ->
                            if todo.id == idTodo then
                                { todo | status = currentStatus }

                            else
                                todo
                        )
                        model.todoList
            in
            { model | todoList = todoList }

        UpdateAllTodoStatuses isChecked ->
            let
                currentStatus =
                    if isChecked == True then
                        CLOSED

                    else
                        OPEN

                todoList =
                    List.map (\todo -> { todo | status = currentStatus }) model.todoList
            in
            { model | todoList = todoList }



---- VIEWS ----


view : Model -> (Msg -> mainMsg) -> Html mainMsg
view model mainMsg =
    Html.main_ [ Attributes.class "todo-list-wrapper" ]
        [ viewAddTodo model.addTodoText model.todoList
        , viewTodoList model.todoList
        ]
        |> Html.map (\msg -> mainMsg msg)


viewAddTodo addTodoText todoList =
    let
        areAllTodosClosed =
            List.length todoList
                > 0
                && List.all (\todo -> todo.status == CLOSED) todoList

        checkboxCheckedClass =
            if areAllTodosClosed == True then
                "add-todo__checkbox--checked"

            else
                ""
    in
    Html.section [ Attributes.class "add-todo" ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.class "add-todo__checkbox"
            , Attributes.class checkboxCheckedClass
            , Attributes.checked areAllTodosClosed
            , Events.onCheck UpdateAllTodoStatuses
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
    let
        classModifier =
            case todo.status of
                OPEN ->
                    "open"

                EDITING _ ->
                    "editing"

                CLOSED ->
                    "closed"
    in
    Html.li
        [ Attributes.class "todo-list-item"
        , Attributes.class <| "todo-list-item--" ++ classModifier
        , Attributes.attribute "data-id" todo.id
        ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.class "todo-list-item__checkbox"
            , Attributes.class <| "todo-list-item__checkbox--" ++ classModifier
            , Attributes.checked <| todo.status == CLOSED
            , Events.onCheck <| UpdateTodoStatus todo.id
            ]
            []
        , Html.div
            [ Attributes.class "todo-list-item__label"
            , Attributes.class <| "todo-list-item__label--" ++ classModifier
            ]
            [ Html.text todo.label ]
        , Html.button
            [ Attributes.class "todo-list-item__destroy"
            , Attributes.class <| "todo-list-item__destroy--" ++ classModifier
            ]
            [ Html.text "X" ]
        ]



---- CUSTOM EVENTS ----


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    Json.map tagger Events.keyCode |> Events.on "keydown"
