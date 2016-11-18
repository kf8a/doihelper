import Html exposing (..)
import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, autofocus, placeholder, classList, checked, href, rel, type_, value)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode exposing (Decoder, decodeValue, succeed, int, string, oneOf, null, list, bool, maybe)
import Json.Decode.Extra exposing ((|:))

import Json.Decode.Pipeline exposing (decode, required)
import Task exposing (..)
import HttpBuilder exposing (..)
import Time exposing (second)
import Http
import String


type alias Model =
  { doi : String
  , title : String
  }


type Msg
 = Fetch
 | Receive Model
 | Change String
 | NoOp


initialModel =
  { doi = "10.1038/nrd842"
  , title = "Nothing"
  }


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Json.Decode.map tagger keyCode)


is13 : Int -> Msg
is13 code =
  if code == 13 then Fetch else NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Fetch ->
      ( model, fetchDoi (Debug.log "model" model.doi))
    Receive doi ->
      ( doi, Cmd.none )
    Change inputString ->
      let
          newModel =  { model | doi = inputString }
      in
         ( newModel, Cmd.none )
    NoOp ->
      ( model, Cmd.none )


-- getDoi : String -> Task (HttpBuilder.Error String) (HttpBuilder.Response Model)
-- getDoi doi = 
--   HttpBuilder.get (doiUrl doi)
--   |> withHeader "Content-Type" "application/json"
--   |> withExpect(Http.expectJson doiDecoder)
--   |> withTimeout (10 * Time.second)
--   |> send Receive
--
getDoi doi =
  doi

fetchDoi : String -> Cmd Msg
fetchDoi doi = 
  Cmd.none
  -- let 
  --     url = doiUrl doi
  -- in
  --    Task.perform (\_ -> NoOp) Receive (getDoi url)


doiUrl : String -> String
doiUrl doi =
  String.append "http://dx.doi.org/" (Debug.log "doil" doi)


doiDecoder : Json.Decode.Decoder Model
doiDecoder =
  decode Model
  |> required "title" Json.Decode.string
  |> required "doi" Json.Decode.string


view : Model -> Html Msg
view model = 
  div [] 
  [ h3 [] [text "DOI lookup"]
  , doiView model
  , button [onClick Fetch ] 
        [text "Lookup DOI"]
  ]


doiView : Model -> Html Msg
doiView model =
  div [] 
  [ input [ class "doi-input"
          ,  placeholder "Enter DOI to lookup"
          , value model.doi
          , autofocus True
          , onInput Change
          , onKeyPress is13
          ] [ ]
  , p [] [text model.title ]
  ]


subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


main =
  Html.program
  { init = (initialModel, Cmd.none)
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
