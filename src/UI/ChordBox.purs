module UI.ChordBox where

import CycleZipper
import Halogen.Component.Profunctor
import Prelude

import Control.Comonad (extract)
import DOM.HTML.SelectionMode (SelectionMode)
import Data.Bifunctor (bimap)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Functor.Product.Nested (Product1)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (rmap)
import Halogen (Component)
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapChildQuery)
import Halogen.Query.InputF (InputF(..))
import UI.ChordInput as CI

data Slot = ChordInputSlot | SubstitutionInputSlot
derive instance eq_slot  :: Eq Slot
derive instance ord_slot :: Ord Slot

data Direction = Up | Down

data Query a =
  ChildMessage Message a
  --SubstitutionInputQuery QI.Message a
  --CurrentFocus (Slot -> a)
  --Focus Slot a
  --Shift Direction a

data Message
  = ChordInputMessage CI.Message

type State m =
  { children :: CycleZipper (H.ParentHTML Query Query Slot m) }

chord_box :: forall m. H.Component HH.HTML Query Unit Message m
chord_box =
  H.parentComponent
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initial :: Unit -> State m
  initial _ =
    { children: CZ1 chord_input_ }
    where chord_input_ = child_component ChordInputSlot ChordInputMessage CI.chord_input

  child_component :: forall o. Slot
                            -> (o -> Message) 
                            -> H.Component HH.HTML Query Unit o m 
                            -> H.ParentHTML Query Query Message m
  child_component slot message comp = HH.slot slot comp_ unit (HE.input ChildMessage)
    where ProComponent comp_ = rmap message (ProComponent comp)

  render :: (State m) -> H.ParentHTML Query Query Slot m
  render state =
    let pi = 3.1415
    in
      HH.div [HP.class_ (HH.ClassName "chord-box")]
        [ HH.button_ [ HH.text "UP" ]
        , extract state.children
        , HH.button_ [ HH.text "DOWN" ] ]

  eval :: Query ~> H.ParentDSL (State m) Query Query Slot Message m
  eval = case _ of
    ChildMessage _ next -> pure next