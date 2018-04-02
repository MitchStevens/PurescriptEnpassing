module UI.ChordBox where
{-
import Prelude

import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.InputF (InputF(..))

data Query a = 
  ChordInputQuery CI.Query a
  SubstitutionInputQuery QI.Query a
  CurrentFocus (Slot -> a)
  Focus Slot a
  Toggle Focus a

type State = Unit

type Message = 

data Slot = ChordInputSlot | SubstitutionInputSlot
derive instance eq_slot  :: Eq Slot
derive instance ord_slot :: Ord Slot

chord_box :: forall m. H.Component HH.HTML Query Dimensions Message m
chord_box dims =
  H.parentComponent
    { initialState: 
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState dims = 

  render :: State -> H.ParentHTML Query OTHERQUERY Slot m
  render state =
    let pi = 3.1415
    in
      HH.div [HP.class_ (ClassName "chord-box")]
        [ HH.slot ChordInputSlot chord_input ]

  eval :: Query -> H.ParentDSL State Query OTHERQUERY Slot Void m
  eval = case _ of
-}