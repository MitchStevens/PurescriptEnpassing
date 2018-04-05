module UI.ChordInput where

import Control.Monad.Aff
import Data.Either
import Data.Tuple
import Dimensions
import Music
import Parser
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Pos

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements (br_, h3_)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { curr_chord :: Either ParseError Chord }

data Query a = TextInput String a | Help a

data Message
  = ChordChange (Either ParseError Chord)
  | HelpClicked

chord_input :: forall m. H.Component HH.HTML Query Unit Message m
chord_input =
  H.component { initialState: initial
    , render
    , eval
    , receiver: const Nothing }
  where

  initial :: Unit -> State
  initial _ = 
    { curr_chord: Left (ParseError "" initialPos) }


  render :: State -> H.ComponentHTML Query
  render state =
    let
      pi = 3.1415
      chord = state.curr_chord
    in
      HH.div [ HP.class_ (H.ClassName "chord-input") ]
        [ h3_ [ HH.text "Chord" ]
        , HH.input [ HE.onValueInput $ HE.input TextInput ]
        , br_
        , HH.span_
          [ either display_error display_chord chord
          , HH.button
            [ HE.onClick (HE.input_ Help) ]
            [ HH.text "?" ]
          ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    TextInput str next -> do
      state <- H.get
      let new_chord = runParser str parse_chord
      let new_state = state { curr_chord = new_chord }
      H.put new_state
      H.raise (ChordChange new_chord)
      pure next
    Help next -> H.raise (HelpClicked) *> pure next


display_error :: forall p i. ParseError -> H.HTML p i
display_error error =
  HH.span
    [ HP.id_ "error_display" ]
    [ HH.text (parseErrorMessage error) ]

display_chord :: forall p i. Chord -> H.HTML p i
display_chord chord =
  HH.span [ HP.class_ (H.ClassName "chord-display") ]
    [ HH.span [ HP.class_ (H.ClassName "root") ] [ HH.text (m.root) ]
    , HH.span [ HP.class_ (H.ClassName "mode") ] [ HH.text (m.mode) ]
    , HH.span [ HP.class_ (H.ClassName "head-ext") ] [ HH.text (m.head_ext) ]
    , HH.span_ (map ext_html m.tail_ext)
    ]
  where
    m = markup chord

    ext_html :: String -> H.HTML p i
    ext_html ext = HH.sup
      [ HP.class_ (H.ClassName "extension")]
      [ HH.text $ ext ]