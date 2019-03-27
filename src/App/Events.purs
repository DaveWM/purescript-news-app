module App.Events where

import Prelude

import App.Routes (Route)
import App.State (State(..), Todos, SearchQuery, NewsResponse)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, Affjax, affjax, defaultRequest, get)
import Pux (EffModel, noEffects)
import Simple.JSON (readJSON)

data Event = PageView Route 
    | IncrementCount
    | MakeRequest
    | ResponseReceived Todos
    | SearchQueryChanged (Maybe SearchQuery)
    | NewsResponseReceived NewsResponse

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

parseResponse :: String -> Maybe NewsResponse
parseResponse s = 
  case readJSON s of
      Right (response :: NewsResponse) ->
        Just response
      Left errors -> 
        Nothing

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp IncrementCount (State st) = noEffects $ State st {count = st.count + 1}
foldp MakeRequest st = 
  {state: st
  ,effects: [ do 
    res <- get "http://jsonplaceholder.typicode.com/users/1/todos" :: Affjax _ String
    _ <- liftEff $ log "response received"
    case (readJSON res.response :: Either _ Todos) of
      Right todos ->
        pure $ Just $ ResponseReceived todos
      Left errors -> do
        _ <- liftEff $ error $ "An error occurred " <> (show errors)
        pure Nothing
   ] }
foldp (ResponseReceived todos) (State st) = noEffects $ State st {todos = todos}
foldp (SearchQueryChanged maybeQuery) (State st) = 
  {state: State st {searchQuery = maybeQuery}
  ,effects: case maybeQuery of 
              Nothing -> []
              Just query -> [ do 
                              res <- get ("https://newsapi.org/v2/everything?q=" <> query <> "&apiKey=a35ce68466704851bec15046387412f6")
                              pure $ NewsResponseReceived <$> parseResponse res.response 
                            ]
  }
foldp (NewsResponseReceived response) (State st) = noEffects $ State st {articles = response.articles}