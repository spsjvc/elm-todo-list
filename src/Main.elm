module Main exposing (..)

import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Text


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type alias Todo =
    { id : Int
    , name : String
    , priority : Int
    , isCompleted : Bool
    }


type alias Model =
    { todos : List Todo
    , idCount : Int
    , inputContent : String
    }


type Msg
    = Add
    | Remove Todo
    | Toggle Todo
    | ChangePriority Todo
    | Change String


init : Model
init =
    { todos = []
    , idCount = 1
    , inputContent = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos =
                    { id = model.idCount
                    , name = model.inputContent
                    , priority = 0
                    , isCompleted = False
                    }
                        :: model.todos
                , idCount = model.idCount + 1
                , inputContent = ""
            }

        Remove todo ->
            { model | todos = List.filter (\t -> t.id /= todo.id) model.todos }

        ChangePriority todo ->
            { model
                | todos =
                    List.map
                        (\t ->
                            { t
                                | priority =
                                    if t.id == todo.id then
                                        if t.priority == 2 then
                                            0
                                        else
                                            t.priority + 1
                                    else
                                        t.priority
                            }
                        )
                        model.todos
            }

        Toggle todo ->
            { model
                | todos =
                    List.map
                        (\t ->
                            { t
                                | isCompleted =
                                    if t.id == todo.id then
                                        not t.isCompleted
                                    else
                                        t.isCompleted
                            }
                        )
                        model.todos
            }

        Change newInputContent ->
            { model | inputContent = newInputContent }


renderTodo : Todo -> Html Msg
renderTodo todo =
    li
        [ style
            [ ( "color"
              , if todo.priority == 1 then
                    "orange"
                else if todo.priority == 2 then
                    "red"
                else
                    "black"
              )
            ]
        ]
        [ div [ onClick (ChangePriority todo) ]
            [ (if todo.isCompleted then
                todo.name
                    |> Text.fromString
                    |> Text.line Text.Through
               else
                todo.name
                    |> Text.fromString
              )
                |> Element.leftAligned
                |> Element.toHtml
            ]
        , button [ onClick (Remove todo) ] [ text "Delete" ]
        , button [ onClick (Toggle todo) ]
            [ text
                (if todo.isCompleted then
                    "✘"
                 else
                    "✓"
                )
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "padding", "2rem" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ h1 [] [ text "Todo list: " ]
        , input
            [ placeholder "Write your new todo"
            , onInput Change
            , value model.inputContent
            , style [ ( "width", "150px" ) ]
            ]
            []
        , button [ onClick Add ] [ text "Add" ]
        , ul [] (List.map (\todo -> renderTodo todo) model.todos)
        ]
