module Todo exposing (Model, Msg(..), Todo, TodoStatus(..), initialModel, update, view)

import Browser.Dom as Dom exposing (Error, focus)
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Random as Random exposing (initialSeed, step)
import Task
import UUID exposing (Seeds, UUID)


type TodoFilter
    = ALL
    | ACTIVE
    | COMPLETED


type TodoStatus
    = OPEN
    | EDITING TodoStatus String
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
    , activeFilter : TodoFilter
    }


type Msg
    = NoOp
    | UpdateAddTodoText String
    | CreateTodo Int
    | UpdateTodoStatus String (Maybe String) Bool
    | UpdateAllTodoStatuses Bool
    | DeleteTodo String
    | UpdateEditingLabel String String
    | FinishEditingLabel String Int
    | SetActiveFilter TodoFilter


initialModel : Model
initialModel =
    { todoList = [], seed = Random.initialSeed 0, addTodoText = "", activeFilter = ALL }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateAddTodoText currentAddTodoText ->
            ( { model | addTodoText = currentAddTodoText }, Cmd.none )

        -- ENTER KEY CODE
        CreateTodo 13 ->
            let
                ( id, newSeed ) =
                    Random.step UUID.generator model.seed

                idAsString =
                    UUID.toString id

                trimmedAddTodoText =
                    String.trim model.addTodoText

                todoList =
                    if String.length trimmedAddTodoText > 0 then
                        Todo idAsString trimmedAddTodoText OPEN :: model.todoList

                    else
                        model.todoList
            in
            ( { model | todoList = todoList, seed = newSeed, addTodoText = "" }, Cmd.none )

        CreateTodo _ ->
            ( model, Cmd.none )

        UpdateTodoStatus idTodo Nothing isClosed ->
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
            ( { model | todoList = todoList }, Cmd.none )

        UpdateTodoStatus idTodo (Just editingString) _ ->
            let
                todoList =
                    List.map
                        (\todo ->
                            if todo.id /= idTodo then
                                todo

                            else
                                { todo | status = EDITING todo.status editingString }
                        )
                        model.todoList
            in
            ( { model | todoList = todoList }, Dom.focus ("editing/" ++ idTodo) |> Task.attempt (\_ -> NoOp) )

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
            ( { model | todoList = todoList }, Cmd.none )

        DeleteTodo idTodo ->
            let
                todoList =
                    List.filter (\todo -> todo.id /= idTodo) model.todoList
            in
            ( { model | todoList = todoList }, Cmd.none )

        UpdateEditingLabel idTodo newEditingString ->
            let
                todoList =
                    List.map
                        (\todo ->
                            if todo.id == idTodo then
                                case todo.status of
                                    OPEN ->
                                        todo

                                    EDITING previousStatus _ ->
                                        { todo | status = EDITING previousStatus newEditingString }

                                    CLOSED ->
                                        todo

                            else
                                todo
                        )
                        model.todoList
            in
            ( { model | todoList = todoList }, Cmd.none )

        FinishEditingLabel idTodo 13 ->
            -- 13 => ENTER
            let
                todoList =
                    model.todoList
                        |> List.map
                            (\todo ->
                                if todo.id == idTodo then
                                    case todo.status of
                                        OPEN ->
                                            todo

                                        EDITING previousStatus newLabel ->
                                            { todo | label = String.trim newLabel, status = previousStatus }

                                        CLOSED ->
                                            todo

                                else
                                    todo
                            )
                        |> List.filter
                            (\todo ->
                                if todo.id == idTodo then
                                    String.length todo.label > 0

                                else
                                    True
                            )
            in
            ( { model | todoList = todoList }, Cmd.none )

        FinishEditingLabel idTodo 27 ->
            -- 27 => ESCAPE
            let
                todoList =
                    List.map
                        (\todo ->
                            if todo.id == idTodo then
                                case todo.status of
                                    OPEN ->
                                        todo

                                    EDITING previousStatus _ ->
                                        { todo | label = todo.label, status = previousStatus }

                                    CLOSED ->
                                        todo

                            else
                                todo
                        )
                        model.todoList
            in
            ( { model | todoList = todoList }, Cmd.none )

        FinishEditingLabel _ _ ->
            ( model, Cmd.none )

        SetActiveFilter filter ->
            ( { model | activeFilter = filter }, Cmd.none )



---- VIEWS ----


view : Model -> (Msg -> mainMsg) -> Html mainMsg
view model mainMsg =
    Html.main_ [ Attributes.class "todo-list-wrapper" ]
        [ viewAddTodo model.addTodoText model.todoList
        , viewTodoList model.todoList model.activeFilter
        , viewTodoFooter model.todoList model.activeFilter
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


viewTodoList todoList activeFilter =
    let
        shownTodoList =
            case activeFilter of
                ALL ->
                    todoList

                ACTIVE ->
                    List.filter (\todo -> todo.status == OPEN) todoList

                COMPLETED ->
                    List.filter (\todo -> todo.status == CLOSED) todoList
    in
    List.map viewTodo shownTodoList |> Html.ul [ Attributes.class "todo-list" ]


viewTodo todo =
    let
        classModifier =
            case todo.status of
                OPEN ->
                    "open"

                EDITING _ _ ->
                    "editing"

                CLOSED ->
                    "closed"

        isEditing =
            case todo.status of
                OPEN ->
                    False

                EDITING _ _ ->
                    True

                CLOSED ->
                    False

        editingLabel =
            case todo.status of
                OPEN ->
                    ""

                EDITING _ label ->
                    label

                CLOSED ->
                    ""
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
            , Events.onCheck <| UpdateTodoStatus todo.id Nothing
            ]
            []
        , Html.div
            [ Attributes.class "todo-list-item__label"
            , Attributes.class <| "todo-list-item__label--" ++ classModifier
            , Events.onDoubleClick <| UpdateTodoStatus todo.id (Just todo.label) False
            ]
            [ Html.text todo.label ]
            |> when (isEditing == False)
        , Html.button
            [ Attributes.class "todo-list-item__destroy"
            , Attributes.class <| "todo-list-item__destroy--" ++ classModifier
            , Events.onClick <| DeleteTodo todo.id
            ]
            [ Html.text "X" ]
            |> when (isEditing == False)

        ---- IS EDITING!!! ----
        , Html.input
            [ Attributes.class "todo-list-item__editing-input"
            , Attributes.class <| "todo-list-item__editing-input--" ++ classModifier
            , Attributes.value editingLabel
            , Attributes.id <| "editing/" ++ todo.id
            , Events.onInput <| UpdateEditingLabel todo.id
            , onKeyDown <| FinishEditingLabel todo.id
            ]
            []
            |> when isEditing
        ]


viewTodoFooter todoList activeFilter =
    let
        todoListLength =
            todoList |> List.length

        closedTodoListLength =
            todoList |> List.filter (\todo -> todo.status == CLOSED) |> List.length

        itemsLeft =
            [ String.fromInt todoListLength
            , if todoListLength == 1 then
                " item "

              else
                " items "
            , "left"
            ]
                |> String.concat
    in
    Html.footer
        [ Attributes.class "todo-footer" ]
        [ Html.div [ Attributes.class "items-left" ] [ itemsLeft |> Html.text ] -- viewFooterTodoCount
        , viewTodoFooterFilters activeFilter -- viewFooterFilters
        , Html.div [ Attributes.class "clear-completed" ]
            -- viewFooterClearButton
            [ [ "Clear completed (", String.fromInt closedTodoListLength, ")" ]
                |> String.concat
                |> Html.text
                |> when (closedTodoListLength > 0)
            ]
        ]
        |> when (todoListLength > 0)


viewTodoFooterFilters activeFilter =
    let
        filters =
            [ ( ALL, "All" ), ( ACTIVE, "Active" ), ( COMPLETED, "Completed" ) ]
                |> List.map (viewTodoFooterFilter activeFilter)
    in
    Html.ul [ Attributes.class "filters" ] filters


viewTodoFooterFilter activeFilter ( filterType, filterLabel ) =
    let
        activeClassName =
            if activeFilter == filterType then
                " filters__filter--active"

            else
                ""

        classNames =
            [ "filters__filter"
            , activeClassName
            ]
                |> String.concat
    in
    Html.li [ Attributes.class classNames, Events.onClick <| SetActiveFilter filterType ] [ Html.text filterLabel ]



---- CUSTOM EVENTS ----


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    Json.map tagger Events.keyCode |> Events.on "keydown"



---- Helper functions ----


empty : Html msg
empty =
    Html.text ""


when : Bool -> Html msg -> Html msg
when shouldRender html =
    if shouldRender == True then
        html

    else
        empty
