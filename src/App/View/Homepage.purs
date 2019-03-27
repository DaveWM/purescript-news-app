module App.View.Homepage where

import Prelude

import App.Events (Event(..))
import App.State (State(..), makeSearchQuery)
import CSS (article)
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Pux.DOM.Events (DOMEvent, onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, p, button, ul, li, input)
import Text.Smolder.HTML.Attributes (className, href, placeholder)
import Text.Smolder.Markup ((!), text, (#!))

view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Awesome News Feed"
    input #! onChange (SearchQueryChanged <<< makeSearchQuery <<< targetValue) ! placeholder "Enter your search query..."
    ul $ do 
      for_ s.articles \article -> do
        li $ text article.title
  
      
    
