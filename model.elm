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
  , secondary_title : String
  , series_title : String
  , abstract : String
  , periodical_full_name :String
  -- , periodical_abbreviation : String
  -- , isbn : String
  -- , publisher_url : String
  -- , full_text : String
  -- , address : String
  -- , city : String
  -- , notes : String
  -- , open_access : Bool
  -- , data_urls : String
  -- , has_acknowledgement : Bool
  -- , annotation : String
  -- , pdf : String
  -- , data_tables : List String
  -- , treatment_areas : List String
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
  | GoToShowPage Int

