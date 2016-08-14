import Html.App exposing (beginnerProgram)
import Html exposing (Html, text, button)
import Html.Events exposing (onClick)
import Debug exposing (log)


type alias Model = Int

type Msg = Clicked


main : Program Never
main =
    beginnerProgram { model = 0, view = view, update = update }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked ->
            let
                l = log "clicked" model
            in
                model

view : Model -> Html Msg
view model =
    button [ onClick Clicked ] [ text "click" ]
