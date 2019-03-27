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
import Text.Smolder.HTML (a, button, div, h1, h4, img, input, li, p, ul)
import Text.Smolder.HTML.Attributes (className, href, placeholder, src)
import Text.Smolder.Markup ((!), text, (#!))

view :: State -> HTML Event
view (State s) =
  div ! className "container" $ do
    h1 $ text "Awesome News Feed"
    input ! className "form-control search-input" #! onChange (SearchQueryChanged <<< makeSearchQuery <<< targetValue) ! placeholder "Enter your search query..."
    div ! className "row" $ do 
      for_ s.articles \article -> do
        div ! className "media article" $ do
          a ! className "align-self-center mr-3" ! href article.url $ 
            img ! src article.urlToImage
          div ! className "media-body" $ do
            h4 ! className "mt-0" $ text article.title
            p $ text article.description
            a ! href article.url $ text "Read More..."

  
      
    
