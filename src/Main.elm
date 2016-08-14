import Html.App exposing (beginnerProgram)
import Html exposing (text)


main =
    beginnerProgram { model = 0, view = view, update = update }


update _ model =
    model


view model =
    text "hello world"
