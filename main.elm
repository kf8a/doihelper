port module Main exposing (..)
import Time
import Html exposing (..)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode

import View exposing (view)
import Model exposing (..)

---"10.1038/nrd842"

initialModel =
  { doi = "10.1007/1-4020-4466-6" 
  , title = ""
  , volume = ""
  , issue = ""
  , container_title = ""
  , page = ""
  , authors = []
  , pub_year = 0
  , pub_type = "Article"
  , state = "published"
  , editors = []
  , publisher = ""
  , secondary_title = ""
  , series_title = ""
  , abstract = ""
  , periodical_full_name = ""
  , periodical_abbreviation = ""
  , isbn = ""
  , publisher_url = ""
  , address = ""
  , city = ""
  , notes = ""
  , open_access = False
  , data_urls = ""
  , has_acknowledgement = False
  , annotation = ""
  , pdf = ""
  , data_tables = []
  , treatment_areas = []
  }


personDecoder : Decode.Decoder Person
personDecoder =
  decode Person
  |> required "family" Decode.string
  |> required "given" Decode.string


pubYearDecoder =
  Decode.string 

doiDecoder : Decode.Decoder Model
doiDecoder =
  decode Model
  |> required "DOI" Decode.string
  |> required "title" Decode.string
  |> optional "volume" Decode.string ""
  |> optional "issue" Decode.string ""
  |> optional "container-title" Decode.string ""
  |> optional "page" Decode.string ""
  |> optional "author" (Decode.list personDecoder) []
  |> requiredAt ["published-print", "date-parts","0","0"] (Decode.int )
  |> hardcoded "article"
  |> hardcoded "published"
  |> optional "editor" (Decode.list personDecoder) []
  |> optional "publisher" Decode.string ""
  |> optional "secondary-title" Decode.string ""
  |> optional "series-title" Decode.string ""
  |> optional "abstract" Decode.string ""
  |> optional "periodical-full-name" Decode.string ""
  |> optional "periodical-abbreviation" Decode.string ""
  |> optional "isbn" Decode.string ""
  |> optional "publisher-url" Decode.string ""
  |> optional "address" Decode.string ""
  |> optional "city" Decode.string ""
  |> optional "notes" Decode.string ""
  |> optional "open_access" Decode.bool False
  |> optional "data-urls" Decode.string ""
  |> optional "has-acknowledgment" Decode.bool False
  |> optional "annotation" Decode.string ""
  |> optional "pdf" Decode.string ""
  |> optional "data-tables" (Decode.list Decode.string) []
  |> optional "treatment-areas" (Decode.list Decode.string) []


handleRequestComplete : Result Http.Error Model -> Msg
handleRequestComplete result =
  case result of
    Ok data ->
      Update data
    Err msg ->
      NoOp

handleSubmitComplete : Result Http.Error () -> Msg
handleSubmitComplete result = 
  case result of 
    Ok data ->
      let
          a = Debug.log "ok" data
      in
         GoToShowPage 1
    Err msg ->
      let
          a = Debug.log "error" msg
      in
         -- NoOp
         GoToShowPage 2

addItem : Model -> Cmd Msg
addItem model =
  HttpBuilder.get (url model)
  |> withHeader "Accept" "application/json"
  |> withTimeout (10 * Time.second)
  |> withExpect (Http.expectJson doiDecoder)
  |> send handleRequestComplete


url : Model -> String
url model =
  String.append "http://dx.doi.org/" model.doi


encodePerson: Person -> Encode.Value
encodePerson person =
  Encode.object
    [ ( "given-name", Encode.string person.given )
    , ( "family-name", Encode.string person.family )
    ]

citationEncoder : Model -> Encode.Value
citationEncoder model =
  Encode.object
    [ ( "doi", Encode.string model.doi )
    , ( "title", Encode.string model.title )
    , ( "volume", Encode.string model.volume )
    , ( "issue", Encode.string model.issue )
    , ( "journal", Encode.string model.container_title )
    , ( "page", Encode.string model.page )
    , ( "workflow_state", Encode.string "published" )
    , ( "publisher", Encode.string model.publisher )
    , ( "pub-year", Encode.int model.pub_year)
    , ( "authors", Encode.list (List.map encodePerson model.authors) )
    , ( "editors", Encode.list (List.map encodePerson model.editors) )
    ]
  -- |> required "author" (Decode.list authorDecoder)
  -- |> requiredAt ["published-print", "date-parts","0","0"] (Decode.int )
  -- |> hardcoded "article"
  -- |> optional "editor" (Decode.list authorDecoder) []


submit : Model -> Cmd Msg
submit model =
  HttpBuilder.post "/"
  |> withHeader "Accept" "application/json"
  |> withJsonBody (Debug.log "post-data" (citationEncoder model))
  |> send handleSubmitComplete

newUrl : Int -> String
newUrl id =
   "/new/" ++ (toString id)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Fetch ->
      ( model, addItem model)
    NoOp ->
      ( model, Cmd.none )
    Update new_model ->
      ( new_model, Cmd.none )
    Change inputString ->
      let
          newModel =  { model | doi = inputString }
      in
         ( newModel, Cmd.none )
    Submit ->
      (model,  submit model)
    GoToShowPage id ->
      ( model,  redirect (newUrl id))
    UpdateTitle inputString ->
      let
          newModel = { model | title = inputString }
      in
         ( newModel, Cmd.none )
    UpdateJournal inputString ->
      let
          newModel = { model | container_title = inputString }
      in
         ( newModel, Cmd.none )
    UpdatePage inputString ->
      let
          newModel = { model | page = inputString }
      in
         ( newModel, Cmd.none )
    UpdateAuthors inputString ->
      Debug.crash "TODO"
    UpdateDOI inputString ->
      let
          newModel = { model | doi = inputString }
      in
         ( newModel, Cmd.none )
    UpdateVolume inputString ->
      let
          newModel = { model | volume = inputString }
      in
         ( newModel, Cmd.none )
    UpdatePubYear inputString ->
      let
          pub = Result.withDefault 0 (String.toInt inputString)
          newModel = { model | pub_year = pub }
      in
         ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

port redirect:  String -> Cmd msg

main =
  Html.program
  { init = (initialModel, Cmd.none)
  , view = View.view
  , update = update
  , subscriptions = subscriptions
  }
