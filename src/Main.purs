module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.AVar
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Dimensions
import Halogen.Aff.Util
import DOM
import DOM.HTML.Types
import Data.Maybe
import Halogen.VDom.Driver
import DOM.Node.ParentNode
import Node.FS
import Network.HTTP.Affjax
import UI.ChordInput

type EffM e a = Eff
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , dom :: DOM
  , exception :: EXCEPTION
  , fs :: FS
  , ref :: REF | e) a

main :: forall e. EffM e Unit
main = runHalogenAff do
  element <- await_guitar
  io <- runUI chord_input unit element
  pure unit

await_guitar :: forall e. Aff (dom :: DOM | e) HTMLElement
await_guitar = do
  awaitLoad
  element <- selectElement (QuerySelector "#content #guitar")
  maybe (throwError (error "Could not find element")) pure element