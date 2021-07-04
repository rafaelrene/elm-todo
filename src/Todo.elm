module Todo exposing (Model, Msg(..), Todo, TodoStatus(..), initialModel, update, view)

import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
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
    }


type Msg
    = CreateTodo String


initialModel : Model
initialModel =
    { todoList = [], seed = Random.initialSeed 0 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CreateTodo label ->
            let
                ( id, newSeed ) =
                    Random.step UUID.generator model.seed

                idAsString =
                    UUID.toString id

                todo =
                    Todo idAsString label OPEN
            in
            { model | todoList = todo :: model.todoList, seed = newSeed }


view : Model -> (Msg -> mainMsg) -> Html mainMsg
view model mainMsg =
    Html.main_ [ Attributes.class "todo-list" ] [ viewAddTodo model ]
        |> Html.map (\msg -> mainMsg msg)


viewAddTodo model =
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
            ]
            []
        ]


setTodoLabel : String -> Todo -> Todo
setTodoLabel newLabel todo =
    { todo | label = newLabel }


updateTodoStatus : TodoStatus -> Todo -> Todo
updateTodoStatus newStatus todo =
    { todo | status = newStatus }


openTodo : Todo -> Todo
openTodo =
    updateTodoStatus OPEN


editTodo : String -> Todo -> Todo
editTodo editLabel =
    updateTodoStatus (EDITING editLabel)


closeTodo : Todo -> Todo
closeTodo =
    updateTodoStatus CLOSED
