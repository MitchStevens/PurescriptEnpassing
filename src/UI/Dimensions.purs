module Dimensions (
  Dimensions,
  dimensions,
  scalar,
  sub_divide
) where

import Prelude
import Data.Tuple
import Data.Int

type Dimensions = 
  { width  :: Number
  , height :: Number }

dimensions :: Number -> Number -> Dimensions
dimensions w h = { width: w, height: h }

scalar :: Number -> Dimensions -> Dimensions
scalar n d = dimensions (d.width * n) (d.height * n)

sub_divide :: Dimensions -> Tuple Int Int -> Dimensions
sub_divide d (Tuple x y) = dimensions (d.width / (toNumber x)) (d.height / (toNumber x))