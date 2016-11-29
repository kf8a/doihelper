module Model exposing  (..)

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
  | GoToShowPage

