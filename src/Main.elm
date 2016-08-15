import Html.App exposing (program)
import Html exposing (Html, text, button)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task
import Dict

type alias Model =
    { lights : List (List Int) }


type Msg = Clicked
        | FetchSucceed Res1
        | FetchFail Http.RawError


type alias Res1 =
    { headers : Dict.Dict String String
    , status : Int
    , statusText : String
    , url : String
    , value : Http.Value
    }


rows : Int
rows = 4

columns : Int
columns = 4

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


decodeJsonLights : String -> Maybe (List (List Int))
decodeJsonLights json =
    let
        decoder = "lights" := Decode.list (Decode.list Decode.int)
    in
        case Decode.decodeString decoder json of
            Ok lights ->
                Just lights
            error ->
                let
                    l = log "decode" error
                in
                    Nothing


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Clicked ->
            let
                l = log "clicked" model
            in
                (model , Cmd.none) 

        FetchSucceed response ->
            let
                l = log "fetch succeed" response
            in
                case response.value of
                    Http.Text responseValue ->
                        let
                            decodedLights = decodeJsonLights responseValue
                            l' = log "lights" decodedLights
                        in
                            case decodedLights of
                                Just lights ->
                                    Model lights ! []
                                _ ->
                                    Model [] ! []
                    _ ->
                        model ! []

        FetchFail res ->
            let
                l = log "fail" res
            in
                model ! []

view : Model -> Html Msg
view model =
    button [ onClick Clicked ] [ text "click" ]




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

        encodedParams = Encode.object [ ("m", Encode.string <| toString columns)
                                      , ("n", Encode.string <| toString rows)
                                      ]
                        |> Encode.encode 0

        request = { verb = "POST"
                  , headers = [("Content-Type", "application/x-www-form-urlencoded")]
                  , url = url
                  , body = Http.string "m=4&n=4"
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform FetchFail FetchSucceed sendRequest
