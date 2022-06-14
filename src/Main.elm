module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Todo exposing (Todo)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { todo : List Todo
    , newTodo : String
    }


init : Model
init =
    { todo = [ Todo "출근하기" True, Todo "퇴근하기" False ]
    , newTodo = ""
    }


type Event
    = TodoUpdate String
    | Submit


update : Event -> Model -> Model
update event model =
    case event of
        TodoUpdate value ->
            { model | newTodo = value }

        Submit ->
            { model | todo = Todo model.newTodo False :: model.todo }


view : Model -> Html Event
view model =
    div [ style "width" "50%", style "margin" "50px auto" ]
        [ h1 [ style "text-align" "center" ]
            [ text "Todo: 할 일을 추가해보세요" ]
        , div [ style "margin-top" "50px", style "text-align" "center" ]
            [ input [ value model.newTodo, onInput TodoUpdate ] []
            , button [ onClick Submit ] [ text "추가" ]
            ]
        , div [ style "border-top" "1px solid black", style "margin-top" "30px" ]
            [ div [ style "margin" "15px auto", style "display" "grid", style "grid-template-columns" "85% 15%", style "border-bottom" "1px solid black", style "padding-bottom" "15px" ]
                [ span []
                    [ text "content" ]
                , span []
                    [ text "status" ]
                ]
            , div [] (L.indexedMap viewTodoItem model.todo)
            ]
        ]


viewTodoItem : Int -> Todo -> Html Event
viewTodoItem index todo =
    div [ style "margin" "15px auto", style "display" "grid", style "grid-template-columns" "85% 15%" ]
        [ span []
            [ text todo.content ]
        , span []
            [ text
                (if todo.check then
                    "완료"

                 else
                    "미완료"
                )
            ]
        ]
