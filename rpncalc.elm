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
  , prompt : Prompt
  }

type alias Prompt = Maybe Int

type Stack
  = Item Int Stack
  | Empty

type Msg
  = Number Int
  | Enter
  | Addition
  | Subtraction
  | Multiplication

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
      { model | stack = stackOp (+) model.stack }
    Subtraction ->
      { model | stack = stackOp (-) model.stack }
    Multiplication ->
      { model | stack = stackOp (*) model.stack }

view : Model -> Html Msg
view model =
  let
    buttonNum n = button [ onClick (Number n) ] [ text (toString n) ]
    buttonEnter = button [ onClick Enter ] [ text "enter" ]
    buttonAdd   = button [ onMouseDown Enter, onMouseUp Addition ] [ text "+" ]
    buttonSub   = button [ onMouseDown Enter, onMouseUp Subtraction ] [ text "-" ]
    buttonMul   = button [ onMouseDown Enter, onMouseUp Multiplication ] [ text "*" ]
  in
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
                          , td [] [ buttonMul ]
                          ]
                  , tr [] [ td [] [ buttonNum 0 ]
                          , td [] [ ]
                          , td [] [ ]
                          , td [] [ buttonEnter ]
                          ]
                  ]
    ]

viewPrompt : Prompt -> Html Msg
viewPrompt prompt =
  let prefix = "> "
  in
    case prompt of
      Nothing -> text prefix
      Just i -> text (prefix ++ toString i)

viewStack : Stack -> List (Html Msg)
viewStack stack =
  let viewStack' stack depth =
    case stack of
      Empty -> []
      Item i stack ->
        let row = toString depth ++ ": " ++ toString i ++ "\n"
        in text row :: viewStack' stack (depth+1)
  in
    viewStack' stack 1

-- Push item into stack
push : Stack -> Int -> Stack
push stack i = Item i stack

-- Pop item from stack
pop : Stack -> (Stack, Maybe Int)
pop stack =
  case stack of
    Empty -> (Empty, Nothing)
    Item i stack -> (stack, Just i)

-- Execute operation on stack
stackOp : (Int -> Int -> Int) -> Stack -> Stack
stackOp f s1 =
    let (s2, value1) = pop s1
        (s3, value2) = pop s2
    in
      case (value1, value2) of
        (Just i1, Just i2) -> push s3 (f i2 i1)
        _ -> Empty
