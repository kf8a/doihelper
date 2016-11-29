module View exposing (view)
import Model exposing (..)
import Regex exposing (..)
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (id, for, class, autofocus, placeholder, classList, checked
                                , href, rel, type_, value, rows, action, method)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick, onWithOptions)


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Decode.map tagger keyCode)


is13 : Int -> Msg
is13 code =
  if code == 13 then Fetch else NoOp

onClickWithoutSubmit msg =
  onWithOptions "click" {stopPropagation = True, preventDefault = True} (Decode.succeed msg)

to_author : Author -> String
to_author author =
  String.join ", " [author.family, author.given] 


to_authors : List Author -> String
to_authors authors =
  String.join "\n" (List.map to_author authors)


--- VIEW


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
