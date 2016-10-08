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
  , prompt : Maybe Int
  }

type Stack
  = Item Int Stack
  | Empty

type Msg
  = Number Int
  | Enter
  | Addition
  | Subtraction

model : Model
model =
  { stack = Empty
  , prompt = Nothing }

update : Msg -> Model -> Model
update msgs model =
  case msgs of
    Number i ->
      case model.prompt of
        Nothing ->
          { model | prompt = Just i }
        Just n ->
          { model | prompt = Just (n*10 + i)}
    Enter ->
      case model.prompt of
        Nothing ->
          model
        Just i ->
          { model | stack = push model.stack i, prompt = Nothing }
    Addition ->
      { model | stack =
          case stackOp (+) model.stack of
            Nothing -> Empty
            Just s -> s
      }
    Subtraction ->
      { model | stack =
          case stackOp (-) model.stack of
            Nothing -> Empty
            Just s -> s
      }

view : Model -> Html Msg
view model =
  div []
    [ pre [] [ viewPrompt model.prompt ]
    , pre [] (viewStack model.stack)
    , table [] [ tr [] [ td [] [ buttonNum 7 ]
                       , td [] [ buttonNum 8 ]
                       , td [] [ buttonNum 9 ]
                       , td [] [ buttonAdd ]
                       ]
               , tr [] [ td [] [ buttonNum 4 ]
                       , td [] [ buttonNum 5 ]
                       , td [] [ buttonNum 6 ]
                       , td [] [ buttonSub ]
                       ]
               , tr [] [ td [] [ buttonNum 1 ]
                       , td [] [ buttonNum 2 ]
                       , td [] [ buttonNum 3 ]
                       ]
               , tr [] [ td [] [ buttonNum 0 ]
                       , td [] [ ]
                       , td [] [ ]
                       , td [] [ buttonEnter ]
                       ]
               ]
    ]

viewPrompt prompt =
  let prefix = "> "
  in
    case prompt of
      Nothing -> text prefix
      Just i -> text (prefix ++ toString i)

viewStack stack =
  let viewStack' stack depth =
    case stack of
      Empty -> []
      Item i stack ->
        let row = toString depth ++ ": " ++ toString i ++ "\n"
        in text row :: viewStack' stack (depth+1)
  in
    viewStack' stack 1

buttonNum n = button [ onClick (Number n) ] [ text (toString n) ]
buttonEnter = button [ onClick Enter ] [ text "enter" ]
buttonAdd = button [ onClick Addition ] [ text "+" ]
buttonSub = button [ onClick Subtraction ] [ text "-" ]

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
            let res = f i2 i1
            in Just (Item res s3)
