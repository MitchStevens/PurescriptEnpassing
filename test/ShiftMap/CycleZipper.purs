module Test.CycleZipper where

import Control.Alt
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import CycleZipper
import Data.Foldable
import Data.Maybe
import Partial.Unsafe
import Prelude
import Data.List hiding (length)
import Test.QuickCheck
--import Text.Formatting
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.LCG

import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Semigroup.Foldable (traverse1_)
import Data.String (joinWith)

{-
  Equality: cz == cz
  Full Rotation Equality: cz == shift (size cz) cz
  No rotation: cz = shift 0 cz == cz
  Inverse: cz == shiftZ (shiftZ cz (-a)) a
  Additive Commutativity: shiftZ a (shiftZ b cz) == shiftZ (a+b) cz
  Inverse RL: rightZ $ leftZ cz == cz
  Inverse LR: leftZ $ rightZ cz == cz


  Size: size cz == length cz
  Insertion inverse L: remove_left  $ insert_left x cz == Just cz
  Insertion inverse R: remove_right $ insert_right x cz == Just cz
  Increase size: insert x => size++
  Decrease size: remove x => size--
-}

all_tests :: forall e. QC e Unit
all_tests = traverse_ quickCheck
  [ equality_test
  , no_rotation_test
  , full_rotation_test
  , inverse_test
  , additive_comm_test
  , inverse_rl_test
  , inverse_lr_test
  
  , size_test
  , insertion_inverse_L_test
  , insertion_inverse_R_test
  , increase_size_test
  , decrease_size_test
  ]

equality_test :: Gen Result
equality_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Equality test failed!"
    (cz ==? cz)

no_rotation_test :: Gen Result
no_rotation_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "No Rotation test failed."
    (shiftN 0 cz ==? cz)

full_rotation_test :: Gen Result
full_rotation_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Full rotation test failed."
    (shiftN (size cz) cz ==? cz)

inverse_test :: Gen Result
inverse_test = do
  (cz :: CycleZipper Int) <- arbitrary
  (n :: Int) <- arbitrary
  pure $ log_fail "Inverse test failed."
    (foldr shiftN cz [n, -n] ==? cz)

additive_comm_test :: Gen Result
additive_comm_test = do
  (cz :: CycleZipper Int) <- arbitrary
  (m :: Int) <- arbitrary
  (n :: Int) <- arbitrary
  pure $ log_fail "Additive commutative test failed."
   (foldr shiftN cz [m, n] ==? shiftN (m+n) cz)

inverse_rl_test :: Gen Result
inverse_rl_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Inverse RL test failed."
    (shift_right (shift_left cz) ==? cz)

inverse_lr_test :: Gen Result
inverse_lr_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Inverse LR test failed."
    (shift_left (shift_right cz) ==? cz)


size_test :: Gen Result
size_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Size test failed"
    (size cz ==? length cz)

insertion_inverse_L_test :: Gen Result
insertion_inverse_L_test = do
  (cz :: CycleZipper Int) <- arbitrary
  (x :: Int) <- arbitrary
  pure $ log_fail "Insertion inverse L test failed."
    (remove_left (insert_left x cz) ==? Just cz)

insertion_inverse_R_test :: Gen Result
insertion_inverse_R_test = do
  (cz :: CycleZipper Int) <- arbitrary
  (x :: Int) <- arbitrary
  pure $ log_fail "Insertion inverse R test failed."
    (remove_right (insert_right x cz) ==? Just cz)

increase_size_test :: Gen Result
increase_size_test = do
  (cz :: CycleZipper Int) <- arbitrary
  (x :: Int) <- arbitrary
  pure $ log_fail "Increase size test failed."
    (size (insert_right x cz) ==? size cz + 1)

decrease_size_test :: Gen Result
decrease_size_test = do
  (cz :: CycleZipper Int) <- arbitrary
  pure $ log_fail "Decrease size test failed."
    case map size (remove_right cz) of
      Just n  -> (size cz - 1) ==? n
      Nothing -> Success


log_fail :: String -> Result -> Result
log_fail str (Failed fail) = Failed (str<>"\n"<>fail)
log_fail _ Success = Success