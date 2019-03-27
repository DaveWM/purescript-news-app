module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, position, backgroundColor, borderRadius, color, display, fontSize, fromString, inlineBlock, key, marginBottom, marginLeft, marginRight, marginTop, padding, px, value, width, (?), height, left, pct)
import CSS.Border (border, solid)
import CSS.Color (darkgray)
import CSS.Display (absolute)
import CSS.Size (px)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, leftTextAlign, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Data.Int (toNumber)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.route of
      (Home) -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white
    textAlign center

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (25.0 #px)
    marginBottom (35.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString ".guide" ? do
    border solid (2.0 #px) green
    color green
    marginRight (10.0 #px)

  fromString ".guide:hover" ? do
    backgroundColor green
    color white

  fromString ".github" ? do
    border solid (2.0 #px) blue
    color blue
    marginLeft (10.0 #px)

  fromString ".github:hover" ? do
    backgroundColor blue
    color white
  
  fromString ".article" ? do
    textAlign leftTextAlign
    marginBottom $ 15.0 #px
    fromString "p" ? do
      marginBottom $ 0.0 #px
    fromString "img" ? do
      width $ 80.0 #px
      height $ 80.0 #px 
      borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
  
  fromString ".search-input" ? do
    marginBottom $ 25.0 #px

  fromString ".loading-spinner.spinner-grow" ? do
    height $ 70.0 #px
    width $ 70.0 #px
    position absolute
    left $ 50.0 #pct
 
