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

colon time =
  let 
    display = (round (Time.inSeconds time)) % 2 == 0
  in
    if display then
      g [ transform "translate(0 3)" ] [
        polygon [ points "0,0 1,0 1,1 0,1 ", fill "lime", stroke "purple", strokeWidth "1"] []
        , polygon [ points "0,3 1,3 1,4 0,4 ", fill "lime", stroke "purple", strokeWidth "1"] []
      ]
    else
      g [] []


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
   g [] [
      digit (val // 10) 0 0
      , digit (val % 10) 1 0
    ]
   
hours time =
  let 
    hour = (round (Time.inHours time) - 1) % 24
  in
    digits hour
   
minutes time =
  let 
    minute = round (Time.inMinutes time) % 60
  in
    digits minute
   
seconds time =
  let 
    minute = round (Time.inSeconds time) % 60
  in
    digits minute

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
        
      , svg [ viewBox "0 0 100 100", width "300px" ]
        [ 
        
        
         g [ transform "translate(0, 0)" ] [ 
            hours model
          ]
        , g [ transform "translate(16,0)"] [
          colon model
        ]
        , g [ transform "translate(20, 0)" ] [ 
            minutes model
          ]
          , g [ transform "translate(36,0)"] [
          colon model
        ]
        , g [ transform "translate(40, 0)" ] [ 
            seconds model
          ]        
        ]
    ]