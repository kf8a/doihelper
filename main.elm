import Time
import Regex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, for, class, autofocus, placeholder, classList, checked
                                , href, rel, type_, value, rows, action, method)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick, onWithOptions)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias Model =
  { doi : String
  , title : String
  , volume : String
  , issue : String
  , container_title : String
  , page : String
  , authors : List Author
  , pub_year : Int
  , pub_type : String
  , state : String
  , editors : List Author
  , publisher : String
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
  | Submit


initialModel =
  { doi = "10.1038/nrd842"
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
  |> optional "volume" Decode.string ""
  |> optional "issue" Decode.string ""
  |> required "container-title" Decode.string
  |> optional "page" Decode.string ""
  |> required "author" (Decode.list authorDecoder)
  |> requiredAt ["published-print", "date-parts","0","0"] (Decode.int )
  |> hardcoded "article"
  |> hardcoded "published"
  |> optional "editor" (Decode.list authorDecoder) []
  |> optional "publisher" Decode.string ""


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

handleSubmitComplete : Result Http.Error () -> Msg
handleSubmitComplete result = 
  case result of 
    Ok data ->
      let
          a = Debug.log "ok" data
      in
         NoOp
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

to_author : Author -> String
to_author author =
  String.join ", " [author.family, author.given] 

to_authors : List Author -> String
to_authors authors =
  String.join "\n" (List.map to_author authors)

view : Model -> Html Msg
view model = 
  div [ class "container" ] 
  [ h3 [] [text "DOI lookup"]
  , div []
    [ doiView model
    , button [ class "btn"
           , onClickWithoutSubmit  Fetch ] 
        [text "Lookup DOI"]
    , citationView model
    , button [ type_ "submit"
             , class "btn bnt-submit"
             , onClick Submit
             ] [ text "Submit" ]
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

authorView : Model -> Html Msg
authorView model =
    div [class "form-group" ] 
      [ label [ for "article-authors"] [ text "Authors" ]
      , textarea [ id "article-authors"
            , class "form-control"
            , rows (List.length model.authors)
            , placeholder "Authors"
            , value (to_authors model.authors)
            , onInput UpdateAuthors ] []
      ]

editorView : Model -> Html Msg
editorView model =
    div [class "form-group" ] 
      [ label [ for "article-editors"] [ text "Editors" ]
      , textarea [ id "article-editors"
            , class "form-control"
            , rows (List.length model.authors)
            , placeholder "editors"
            , value (to_authors model.editors)
            ] []
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

noModelRow : String -> String -> Html Msg
noModelRow field hint =
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
            , value field ] []
      ]


citationView: Model -> Html Msg
citationView model =
  div []
    [ citationViewRow model.title "Article Title" (onInput UpdateTitle)
    , citationViewRow model.container_title "Journal" (onInput UpdateJournal)
    , authorView model
    , citationViewRow model.page "Pages" (onInput UpdatePage)
    , citationViewRow model.volume "Volume" (onInput UpdateVolume)
    , citationViewRow (toString model.pub_year) "Year" (onInput UpdatePubYear)
    , editorView model
    , noModelRow model.publisher "Publisher"
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
