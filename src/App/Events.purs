module App.Events where

import Prelude

import App.Routes (Route)
import App.State (State(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, Affjax, affjax, defaultRequest, get)
import Pux (EffModel, noEffects)

data Event = PageView Route 
    | IncrementCount
    | MakeRequest
    | ResponseReceived String

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp IncrementCount (State st) = noEffects $ State st {count = st.count + 1}
foldp MakeRequest st = 
  {state: st
  ,effects: [ do 
    res <- get "http://jsonplaceholder.typicode.com/users/1/todos" :: Affjax _ String
    l <- liftEff $ log ("Response: " <> res.response)
    pure $ Just (ResponseReceived res.response) ] }
foldp (ResponseReceived response) (State st) = noEffects $ State st

