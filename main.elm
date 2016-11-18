import Time
import Regex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, for, class, autofocus, placeholder, classList, checked
                                , href, rel, type_, value, rows, action)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick, onWithOptions)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt)


type alias Model =
  { doi : String
  , title : String
  , volume : String
  , issue : String
  , container_title : String
  , page : String
  , authors : List Author
  , pub_year : Int
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
  | UpdatePubYear String


initialModel =
  { doi = "10.1038/nrd842"
  , title = ""
  , volume = ""
  , issue = ""
  , container_title = ""
  , page = ""
  , authors = []
  , pub_year = 0
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

pubYearDecoder =
  Decode.string 

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
  |> requiredAt ["published-print", "date-parts","0","0"] (Decode.int )


handleRequestComplete : Result Http.Error Model -> Msg
handleRequestComplete result =
  case result of
    Ok data ->
      Update (Debug.log "result" data)
    Err msg ->
      let
          a = Debug.log "error" msg
      in
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

onClickWithoutSubmit msg =
  onWithOptions "click" {stopPropagation = True, preventDefault = True} (Decode.succeed msg)

view : Model -> Html Msg
view model = 
  div [ class "container" ] 
  [ h3 [] [text "DOI lookup"]
  , form [action "/"] 
    [ doiView model
    , button [ class "btn"
           , onClickWithoutSubmit  Fetch ] 
        [text "Lookup DOI"]
    , citationView model
    , button [ class "btn bnt-submit" ] [ text "Submit" ]
    ]
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

to_author : Author -> String
to_author author =
  String.join ", " [author.family, author.given] 

to_authors : List Author -> String
to_authors authors =
  String.join "\n" (List.map to_author authors)

authorView : Model -> Html Msg
authorView model =
    div [class "form-group" ] 
      [ label [ for "article-authors"] [ text "Authors" ]
      , textarea [ id "article-authors"
            , class "form-control"
            , rows 3
            , placeholder "Authors"
            , value (to_authors model.authors)
            , onInput UpdateAuthors ] []
      ]


citationViewRow : String -> String -> Attribute Msg ->  Html Msg
citationViewRow field hint action =
  let
    dashified = replace All (regex "\\s") (\_ -> "-") hint
    my_id = String.append "article-" (String.toLower dashified)
  in
    div [class "form-group" ] 
      [ label [ for my_id ] [ text hint]
      , input [ id my_id
            , class "form-control"
            , type_ "text"
            , placeholder hint
            , value field 
            , action ] []
      ]


citationView: Model -> Html Msg
citationView model =
  div []
    [ authorView model
    , citationViewRow model.title "Article Title" (onInput UpdateTitle)
    , citationViewRow model.container_title "Journal" (onInput UpdateJournal)
    , citationViewRow model.page "Pages" (onInput UpdatePage)
    , citationViewRow model.volume "Volume" (onInput UpdateVolume)
    , citationViewRow (toString model.pub_year) "Year" (onInput UpdatePubYear)
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
