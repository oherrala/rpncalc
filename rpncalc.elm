import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import List as List

main =
  App.beginnerProgram { model = model
                      , view = view
                      , update = update }

type alias Model =
  { stack : Stack
  }

type Stack
  = Item Int Stack
  | Empty

type Msg
  = Number Int
  | Addition
  | Subtraction

model : Model
model =
  { stack = Empty }

update : Msg -> Model -> Model
update msgs model =
  case msgs of
    Number i ->
      { stack = push model.stack i }
    Addition ->
      { stack =
          case stackOp (+) model.stack of
            Nothing -> Empty
            Just s -> s
      }
    Subtraction ->
      { stack =
          case stackOp (-) model.stack of
            Nothing -> Empty
            Just s -> s
      }

view : Model -> Html Msg
view model =
  div []
    [ span [] [ text (toString model) ]
    , div [] (List.map buttonNum [0..9])
    , div [] [ button [ onClick Addition ] [ text "+" ]
             , button [ onClick Subtraction ] [ text "-" ]
             ]
    ]

buttonNum n = button [ onClick (Number n) ] [ text (toString n) ]


-- Push item into stack
push : Stack -> Int -> Stack
push stack i = Item i stack

-- Pop item from stack
pop : Stack -> Maybe (Stack, Int)
pop stack =
  case stack of
    Empty -> Nothing
    Item i stack -> Just (stack, i)

-- Execute operation on stack
stackOp : (Int -> Int -> Int) -> Stack -> Maybe Stack
stackOp f s1 =
    case pop s1 of
      Nothing -> Nothing
      Just (s2, i1) ->
        case pop s2 of
          Nothing -> Nothing
          Just (s3, i2) ->
            let res = f i1 i2
            in Just (Item res s3)
