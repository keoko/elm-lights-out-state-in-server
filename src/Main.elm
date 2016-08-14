import Html.App exposing (beginnerProgram)
import Html exposing (text, button)
import Html.Events exposing (onClick)
import Debug exposing (log)


type Msg = Clicked


main =
    beginnerProgram { model = 0, view = view, update = update }


update msg model =
    case msg of
        Clicked ->
            let
                l = log "clicked" model
            in
                model


view model =
    button [ onClick Clicked ] [ text "click" ]
