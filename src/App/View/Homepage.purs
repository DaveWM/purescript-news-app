module App.View.Homepage where

import Prelude

import App.Events (Event(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (const, ($))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, p, button)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text, (#!))

view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Pux"
    p $ text ("Count is: " <> (show s.count)) 
    button #! onClick (const IncrementCount) $ text "Inc"
    button #! onClick (const MakeRequest) $ text "Load"
