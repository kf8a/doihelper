import Time
import Html exposing (..)
import Html.Attributes exposing (id, for, class, autofocus, placeholder, classList, checked, href, rel, type_, value)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)

type alias Model =
  { doi : String
  , title : String
  , volume : String
  , issue : String
  , container_title : String
  , page : String
  , authors : List Author
  }

type alias Author = 
  { family : String
  , given : String
  }

type Msg
  = NoOp
  | Fetch
  | Update Model
  | Change String
  | UpdateTitle String
  | UpdateJournal String
  | UpdatePage String
  | UpdateVolume String
  | UpdateAuthors String
  | UpdateDOI String


initialModel =
  { doi = "10.1038/nrd842"
  , title = "Nothing"
  , volume = "0"
  , issue = "0"
  , container_title = ""
  , page = ""
  , authors = []
  }

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Decode.map tagger keyCode)


is13 : Int -> Msg
is13 code =
  if code == 13 then Fetch else NoOp

authorDecoder : Decode.Decoder Author
authorDecoder =
  decode Author
  |> required "family" Decode.string
  |> required "given" Decode.string


doiDecoder : Decode.Decoder Model
doiDecoder =
  decode Model
  |> required "DOI" Decode.string
  |> required "title" Decode.string
  |> required "volume" Decode.string
  |> required "issue" Decode.string
  |> required "container-title" Decode.string
  |> required "page" Decode.string
  |> required "author" (Decode.list authorDecoder)


handleRequestComplete : Result Http.Error Model -> Msg
handleRequestComplete result =
  case result of
    Ok data ->
      Update (Debug.log "result" data)
    Err msg ->
      NoOp


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
          newModel = {model | page = inputString }
      in
         ( newModel, Cmd.none )
    UpdateAuthors inputString ->
      Debug.crash "TODO"
    UpdateDOI inputString ->
      let
          newModel = {model | doi = inputString }
      in
         ( newModel, Cmd.none )
    UpdateVolume inputString ->
      let
          newModel = {model | volume = inputString }
      in
         ( newModel, Cmd.none )


view : Model -> Html Msg
view model = 
  div [ class "container" ] 
  [ h3 [] [text "DOI lookup"]
  , doiView model
  , button [ class "btn btn-default"
           , onClick Fetch ] 
        [text "Lookup DOI"]
  , citationView model
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
  , p [] [ text model.title 
         , text model.page
         , text model.volume
         ]
  ]

citationViewRow : String ->  Html Msg
citationViewRow field =
  div [class "form-group" ] 
    [ label [ for "article-title" ] [ text "Title" ]
    , input [ id "article-title"
          , class "form-control"
          , type_ "text"
          , placeholder "Article Title" 
          , value field 
          , onInput UpdateTitle ] []
    ]


citationView: Model -> Html Msg
citationView model =
  div []
    [ citationViewRow model.title 
    , div [class "form-group" ] 
        [ label [ for "article-title" ] [ text "Title" ]
        , input [ id "article-title"
                , class "form-control"
                , type_ "text"
                , placeholder "Article Title" 
                , value model.title
                ,  onInput UpdateTitle ] []
        ]
    , div [ class "form-group" ]
        [ label [ for "journal-title" ] [ text "Journal" ]
        , input [ id "journal-title"
                , class "form-control"
                , type_ "text"
                , placeholder "Journal"
                , value model.container_title
                , onInput UpdateJournal ] []
        ]
    , div [ class "form-group" ]
        [ label [ for "journal-page" ] [ text "Pages" ]
        , input [ id "journal-page"
                , class "form-control"
                , type_ "text"
                , placeholder "Pages"
                , value model.page
                , onInput UpdatePage ] []
        ]
    , div [ class "form-group" ]
        [ label [ for "journal-volume" ] [ text "Volume" ]
        , input [ id "journal-volume"
                , class "form-control"
                , type_ "text"
                , placeholder "Volume"
                , value model.volume
                , onInput UpdateVolume ] []
        ]
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
