module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.List.Lazy (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

type SearchQuery = String

makeSearchQuery :: String -> Maybe SearchQuery
makeSearchQuery "" = Nothing
makeSearchQuery s = Just s

type Article = 
  { title :: String

  , url :: String
  , description :: String}

type NewsResponse = 
  { articles :: Array Article}

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , searchQuery :: Maybe SearchQuery
  , articles :: Array Article
  }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , searchQuery: Nothing
  , articles: []
  }
