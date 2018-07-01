import Html exposing (Html, div, h1)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)


-- UPDATE

type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW
  

polyh x y = 
  polygon [points (toString (x + 0) ++ "," ++ toString ( y + 1) ++ " " ++
  toString (x + 1) ++ "," ++ toString ( y + 0) ++ " " ++
  toString (x + 3) ++ "," ++ toString ( y + 0) ++ " " ++
  toString (x + 4) ++ "," ++ toString ( y + 1) ++ " " ++
  toString (x + 3) ++ "," ++ toString ( y + 2) ++ " " ++
  toString (x + 1) ++ "," ++ toString ( y + 2)), fill "lime", stroke "purple", strokeWidth "1"] []

polyv x y = 
  polygon [points (toString (x + 0) ++ "," ++ toString ( y + 1) ++ " " ++
  toString (x + 1) ++ "," ++ toString ( y + 0) ++ " " ++
  toString (x + 2) ++ "," ++ toString ( y + 1) ++ " " ++
  toString (x + 2) ++ "," ++ toString ( y + 3) ++ " " ++
  toString (x + 1) ++ "," ++ toString ( y + 4) ++ " " ++
  toString (x + 0) ++ "," ++ toString ( y + 3)), fill "lime", stroke "purple", strokeWidth "1"] []

digit value x y =
  let
    offset = transform ("translate(" ++ toString (x * 8) ++ " " ++ toString (y * 12) ++ ")")
  in
  case value of
  0 -> g [ offset ] [
      polyv 0 1
    , polyv 0 5
    , polyv 4 1
    , polyv 4 5

    , polyh 1 0
    , polyh 1 8
  ]
  1 -> g [ offset ] [
      polyv 4 1
    , polyv 4 5
  ]
  2 -> g [ offset ] [
      polyv 4 1
    , polyv 0 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  3 -> g [ offset ] [
      polyv 4 1
    , polyv 4 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  4 -> g [ offset ] [
      polyv 0 1
    , polyv 4 1
    , polyv 4 5

    , polyh 1 4
  ]
  5 -> g [ offset ] [
      polyv 0 1
    , polyv 4 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  6 -> g [ offset ] [
      polyv 0 1
    , polyv 0 5
    , polyv 4 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  7 -> g [ offset ] [
      polyv 4 1
    , polyv 4 5

    , polyh 1 0
  ]
  8 -> g [ offset ] [
      polyv 0 1
    , polyv 0 5
    , polyv 4 1
    , polyv 4 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  9 -> g [ offset ] [
      polyv 0 1
    , polyv 4 1
    , polyv 4 5

    , polyh 1 0
    , polyh 1 4
    , polyh 1 8
  ]
  _ -> g [ offset ] [
      polyv 0 1
      ]

mod value cycle = value % cycle 

digits val = 
  if val < 10 then
    g [] [
      digit 0 0 0
      , digit val 1 0
    ]
  else
    g [] [
      digit (val // 10) 0 0
      , digit (val % 10) 1 0
    ]

view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    div [] [
        h1 [] [ text (toString (mod (round (Time.inSeconds model)) 60)) ]
      , svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
        , polygon [points "20,10 25,19 16,21", fill "lime", stroke "purple", strokeWidth "1"] []
        
        , g [ transform "translate(0, 1)" ] [ 
            digits (round (Time.inMinutes model))
          ]
        , g [ transform "translate(15, 1)" ] [ 
            digits (mod (round (Time.inSeconds model)) 60)
          ]        
        ]
    ]