module Test.CycleZipper where

import Prelude
import CycleZipper
import Data.Foldable
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

{-
  Equality: cz == cz
  Full Rotation Equality: cz == shift (size cz) cz
  Inverse: cz == shiftZ (shiftZ cz (-a)) a
  Additive Commutativity: shiftZ a (shiftZ b cz) == shiftZ (a+b) cz
  Inverse RL: rightZ $ leftZ cz == cz
  Inverse LR: leftZ $ rightZ cz == cz
-}

type Test a b = Arbitrary a => Eq a => Show a => b

int_tests :: forall e. Test Int (QC e Unit)
int_tests = run_tests

string_tests :: forall e. Test String (QC e Unit)
string_tests = run_tests

run_tests :: forall a e. Test a (QC e Unit)
run_tests = quickCheck equality_test
{-
  [ equality_test
  , full_rotation_test
  , inverse_test
  , additive_comm_test
  , inverse_rl_test
  , inverse_lr_test ]
-}

equality_test :: forall a. Test a (Gen Result)
equality_test = do
  (cz :: CycleZipper a) <- arbitrary
  pure $ cz ==? cz

full_rotation_test :: forall a. Test a (Gen Result)
full_rotation_test = do
  (cz :: CycleZipper a) <- arbitrary
  pure $ shiftZ cz (size cz) ==? cz

inverse_test :: forall a. Test a (Gen Result)
inverse_test = do
  (cz :: CycleZipper a) <- arbitrary
  (n :: Int) <- arbitrary
  pure $ foldl shiftZ cz [n, -n] ==? cz

additive_comm_test :: forall a. Test a (Gen Result)
additive_comm_test = do
  (cz :: CycleZipper a) <- arbitrary
  (m :: Int) <- arbitrary
  (n :: Int) <- arbitrary
  pure $ foldl shiftZ cz [m, n] ==? shiftZ cz (m+n)

inverse_rl_test :: forall a. Test a (Gen Result)
inverse_rl_test = do
  (cz :: CycleZipper a) <- arbitrary
  pure $ rightZ (leftZ cz) ==? cz

inverse_lr_test :: forall a. Test a (Gen Result)
inverse_lr_test = do
  (cz :: CycleZipper a) <- arbitrary
  pure $ leftZ (rightZ cz) ==? cz

