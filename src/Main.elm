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
                    , isCompleted = False
                    }
                        :: model.todos
                , idCount = model.idCount + 1
                , inputContent = ""
            }

        Remove todo ->
            { model | todos = List.filter (\t -> t.id /= todo.id) model.todos }

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
    li [ onClick (Toggle todo) ]
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
        , button [ onClick (Remove todo) ] [ text "X" ]
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
        , ul [] (List.map (\todo -> renderTodo todo) model.todos)
        , input
            [ placeholder "Write your new todo"
            , onInput Change
            , value model.inputContent
            , style [ ( "width", "150px" ) ]
            ]
            []
        , br [] []
        , button [ onClick Add ] [ text "Add" ]
        ]
