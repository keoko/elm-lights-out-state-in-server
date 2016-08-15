import Html.App exposing (program)
import Html exposing (Html, text, button, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Http
import Json.Decode as Decode exposing ((:=))
import Task
import Dict


type alias Lights = List (List Int) 

type alias Model =
    { lights : Lights }


type Msg = RequestSucceed HttpResponse
         | RequestFailed Http.RawError
         | ToggleLight Point


type alias Point = (Int, Int)


type alias HttpResponse =
    { headers : Dict.Dict String String
    , status : Int
    , statusText : String
    , url : String
    , value : Http.Value
    }


rows : Int
rows = 3

columns : Int
columns = 3

main : Program Never
main =
    program { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


init : (Model, Cmd Msg)
init =
    ( Model [], requestResetLights )


decodeJsonLights : String -> Maybe Lights
decodeJsonLights json =
    let
        decoder = "lights" := Decode.list (Decode.list Decode.int)
    in
        case Decode.decodeString decoder json of
            Ok lights ->
                Just lights
            error ->
                Nothing


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RequestSucceed response ->
            case response.value of
                Http.Text responseValue ->
                    case (decodeJsonLights responseValue) of
                        Just lights ->
                            Model lights ! []
                        _ ->
                            model ! []
                _ ->
                    model ! []

        RequestFailed response ->
            let
                l = log "http request failed" response
            in
                model ! []

        ToggleLight point ->
            (model, requestToggleLight point)


-- VIEW

viewLight : Point -> Int -> Html Msg
viewLight (columnIndex, rowIndex) lightOn =
    td [ style [ ("background", if lightOn == 1 then "blue" else "black" )
                , ("width", "10vw")
                , ("height", "10vw")
                , ("margin", "0.1vw")
                ]

        , onClick <| ToggleLight (columnIndex, rowIndex) ] []


viewRow : Int -> List Int -> Html Msg
viewRow rowIndex rowLights =
    tr [] ( List.indexedMap (\columnIndex lights -> viewLight (rowIndex, columnIndex) lights) rowLights )


view : Model -> Html Msg
view model =
    table []
        (List.indexedMap viewRow model.lights)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP

requestResetLights : Cmd Msg
requestResetLights =
    let
        url =
            "http://localhost:3000/reset-lights"

        encodedParams = "m=" ++ (toString rows) ++ "&n=" ++ (toString columns)

        request = { verb = "POST"
                  , headers = [("Content-Type", "application/x-www-form-urlencoded")]
                  , url = url
                  , body = Http.string encodedParams
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform RequestFailed RequestSucceed sendRequest


requestToggleLight : Point -> Cmd Msg
requestToggleLight (x,y) =
    let
        url =
            "http://localhost:3000/flip-light"

        encodedParams = "x=" ++ (toString x) ++ "&y=" ++ (toString y)

        request = { verb = "POST"
                  , headers = [("Content-Type", "application/x-www-form-urlencoded")]
                  , url = url
                  , body = Http.string encodedParams
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform RequestFailed RequestSucceed sendRequest
