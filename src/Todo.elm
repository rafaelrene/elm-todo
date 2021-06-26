module Todo exposing (Msg(..), Todo, TodoStatus(..), closeTodo, editTodo, openTodo, setTodoLabel, update, view)

import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import Random as Random exposing (initialSeed, step)
import UUID exposing (UUID)


type TodoStatus
    = OPEN
    | EDITING String
    | CLOSED


type alias Todo =
    { id : String
    , label : String
    , status : TodoStatus
    }


type Msg
    = CreateTodo String


update : Msg -> Todo
update msg =
    case msg of
        CreateTodo label ->
            let
                id =
                    Random.step UUID.generator (Random.initialSeed 12345)
                        |> Tuple.first
                        |> UUID.toString
            in
            Todo id label OPEN


view todoLabel =
    Html.div
        [ Events.onClick (CreateTodo todoLabel) ]
        [ Html.text todoLabel ]


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
