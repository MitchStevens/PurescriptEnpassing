module CycleZipper where

import Data.List hiding (null, length)
import Prelude
import Data.Foldable
import Data.Semigroup.Foldable
import Control.Comonad
import Data.Tuple
import Data.NonEmpty as N
import Data.List.Types
import Data.Bifunctor
import Partial
import Data.Maybe
import Partial.Unsafe
import Test.QuickCheck.Arbitrary

type CycleZipperType a =
  { fs :: List a
  , lenf :: Int
  , value :: a
  , rs :: List a
  , lenr :: Int }

data CycleZipper a
  = CZ1 a
  | CZ2 { value :: a, other :: a }
  | CZN
    { fs :: NonEmptyList a
    , lenf :: Int
    , value :: a
    , rs :: NonEmptyList a
    , lenr :: Int }

instance eq_cz :: Eq a => Eq (CycleZipper a) where
  eq cz1 cz2 = (to_list cz1) == (to_list cz2)

instance show_cz :: Show a => Show (CycleZipper a) where
  show (CZ1 v)  = fold ["[(", show v, ")]"]
  show (CZ2 cz) = fold ["[(", show cz.value, "), ", show cz.other, "]"]
  show (CZN cz) = "[" <> fs <> ", (" <> show cz.value <> "), " <> rs <> "]"
    where
      fs = intercalate ", " $ reverse $ map show $ toList cz.fs
      rs = intercalate ", " $ map show cz.rs
instance functor_cz :: Functor CycleZipper where
  map f (CZ1 v)  = CZ1 (f v)
  map f (CZ2 cz) = CZ2 { value: f cz.value, other: f cz.other }
  map f (CZN cz) = CZN
    { fs: map f cz.fs
    , lenf: 0
    , value: f cz.value
    , rs: map f cz.rs
    , lenr: 0 }
instance foldable_cz :: Foldable CycleZipper where
  foldl f z (CZ1 v)  = f z v
  foldl f z (CZ2 cz) = f (f z cz.value) cz.other
  foldl f z (CZN cz) = foldl f (foldl f (f z cz.value) cz.rs) cz.fs
  foldr f z (CZ1 v)  = f v z
  foldr f z (CZ2 cz) = f cz.other (f cz.value z)
  foldr f z (CZN cz) = foldr f (foldr f (f cz.value z) cz.rs) cz.fs
  foldMap f (CZ1 v)  = f v
  foldMap f (CZ2 cz) = f cz.value <> f cz.other
  foldMap f (CZN cz) = f cz.value <> foldMap f cz.rs <> foldMap f cz.fs
instance foldable1_cz :: Foldable1 CycleZipper where
  fold1 (CZ1 value) = value
  fold1 (CZ2 cz) = cz.value <> cz.other
  fold1 (CZN cz) = foldr append (foldr append cz.value cz.rs) cz.fs
  foldMap1 = foldMap1Default
instance extend_lz :: Extend CycleZipper where
  extend f = CZ1 <<< f
instance comonad_lz :: Comonad CycleZipper where
  extract (CZ1 v)  = v
  extract (CZ2 cz) = cz.value
  extract (CZN cz) = cz.value
instance arbitrary_cz :: Arbitrary a => Arbitrary (CycleZipper a) where
  arbitrary = map from_list arbitrary

from_list :: NonEmptyList ~> CycleZipper
from_list (NonEmptyList (N.NonEmpty v vs)) = case vs of
  Nil -> CZ1 v
  Cons o Nil -> CZ2 { value: v, other: o }
  _ -> cycle_zipper
    { fs: Nil
    , lenf: 0
    , value: v
    , rs: vs
    , lenr: length vs }

to_list :: CycleZipper ~> List
to_list (CZ1 v)  = singleton v
to_list (CZ2 cz)  = Cons cz.value (Cons cz.other Nil)
to_list (CZN cz) = Cons cz.value (toList cz.rs <> reverse (toList cz.fs))

size :: forall a. CycleZipper a -> Int
size (CZ1 _)  = 1
size (CZ2 _)  = 2
size (CZN cz) = cz.lenf + 1 + cz.lenr

{-  Assuming: length fs + length rs >= 2 -}
cycle_zipper :: CycleZipperType ~> CycleZipper
cycle_zipper cz
  | length cz.fs + length cz.rs <= 1 = case (cz.fs <> cz.rs) of
    Cons o Nil -> CZ2 { value: cz.value, other: o}
    _          -> CZ1 cz.value
  | null cz.fs = unsafePartial $ cycle_right cz
  | null cz.rs = unsafePartial $ cycle_left  cz
  | otherwise  = unsafePartial $ CZN
    { fs: to_non_empty cz.fs
    , lenf: cz.lenf
    , value: cz.value 
    , rs: to_non_empty cz.rs
    , lenr: cz.lenr }

{-  Assuming: fs is empty
    Assuming: length rs >= 2 -}
cycle_right :: Partial => CycleZipperType ~> CycleZipper
cycle_right cz = CZN
  { fs: to_non_empty fs
  , lenf: cz.lenr - div cz.lenr 2
  , value: cz.value
  , rs: to_non_empty rs
  , lenr: div cz.lenr 2 }
  where Tuple fs rs = middle cz.rs (div cz.lenr 2)

{-  Assuming that rs is empty
    Assuming that fs >= 2 -}
cycle_left :: Partial => CycleZipperType ~> CycleZipper
cycle_left cz = CZN
  { fs: to_non_empty fs
  , lenf: div cz.lenf 2
  , value: cz.value
  , rs: to_non_empty rs
  , lenr: cz.lenf - div cz.lenf 2 }
  where Tuple rs fs = middle cz.fs (div cz.lenf 2)

middle :: forall a. Partial => List a -> Int -> Tuple (List a) (List a)
middle list n_ = bimap reverse reverse $ middle_rec (Tuple list Nil) n_
  where
    middle_rec :: Tuple (List a) (List a) -> Int -> Tuple (List a) (List a)
    middle_rec tuple 0 = tuple
    middle_rec (Tuple xxs ys) n = case xxs of
      Nil       -> crashWith $ crash (show n)
      Cons x xs -> middle_rec (Tuple xs (Cons x ys)) (n-1)

    crash str = "The middle value "<> str <>" is >= than the size of the list."

to_non_empty :: Partial => List ~> NonEmptyList
to_non_empty = case _ of
  Cons x xs -> NonEmptyList (N.NonEmpty x xs)
  Nil       -> crashWith "List is not NonEmpty"

insert_left :: forall a. a -> CycleZipper a -> CycleZipper a
insert_left x (CZ1 v)  = CZ2 { value: v, other: x }
insert_left x (CZ2 cz) = CZN
  { fs: NonEmptyList $ N.singleton x
  , lenf: 1
  , value: cz.value
  , rs: NonEmptyList $ N.singleton cz.other
  , lenr: 1 }
insert_left x (CZN cz) = CZN
  { fs: nelCons x cz.fs
  , lenf: cz.lenf + 1
  , value: cz.value
  , rs: cz.rs
  , lenr: cz.lenr }

insert_right :: forall a. a -> CycleZipper a -> CycleZipper a
insert_right x (CZ1 v)  = CZ2 { value: v, other: x }
insert_right x (CZ2 cz) = CZN
  { fs: NonEmptyList $ N.singleton cz.other
  , lenf: 1
  , value: cz.value
  , rs: NonEmptyList $ N.singleton x
  , lenr: 1 }
insert_right x (CZN cz) = CZN
  { fs: cz.fs
  , lenf: cz.lenf
  , value: cz.value
  , rs: nelCons x cz.rs
  , lenr: cz.lenr + 1 }

remove_left :: forall a. CycleZipper a -> Maybe (CycleZipper a)
remove_left (CZ1 _)  = Nothing
remove_left (CZ2 cz) = Just $ CZ1 cz.value
remove_left (CZN cz) =
  let NonEmptyList (N.NonEmpty f fs) = cz.fs
      NonEmptyList (N.NonEmpty r rs) = cz.rs 
  in Just $ cycle_zipper
    { fs: fs
    , lenf: cz.lenf - 1
    , value: cz.value
    , rs: r : rs
    , lenr: cz.lenr }

remove_right :: forall a. CycleZipper a -> Maybe (CycleZipper a)
remove_right (CZ1 _)  = Nothing
remove_right (CZ2 cz) = Just $ CZ1 cz.value
remove_right (CZN cz) =
  let NonEmptyList (N.NonEmpty f fs) = cz.fs
      NonEmptyList (N.NonEmpty r rs) = cz.rs
  in Just $ cycle_zipper
    { fs: f : fs
    , lenf: cz.lenf
    , value: cz.value
    , rs: rs
    , lenr: cz.lenr - 1 }

shift_left :: CycleZipper ~> CycleZipper
shift_left (CZ1 v)  = CZ1 v
shift_left (CZ2 cz) = CZ2 { value: cz.other, other: cz.value }
shift_left (CZN cz) = 
  let NonEmptyList (N.NonEmpty f fs) = cz.fs
      NonEmptyList (N.NonEmpty r rs) = cz.rs 
  in cycle_zipper
    { fs: fs
    , lenf: cz.lenf - 1
    , value: f
    , rs: cz.value : r : rs
    , lenr: cz.lenr + 1 }

shift_right :: CycleZipper ~> CycleZipper
shift_right (CZ1 v)  = CZ1 v
shift_right (CZ2 cz) = CZ2 { value: cz.other, other: cz.value }
shift_right (CZN cz) =
  let NonEmptyList (N.NonEmpty f fs) = cz.fs
      NonEmptyList (N.NonEmpty r rs) = cz.rs
  in cycle_zipper
    { fs: cz.value : f : fs
    , lenf: cz.lenf + 1
    , value: r
    , rs: rs
    , lenr: cz.lenr - 1 }

shiftN :: forall a. Int -> CycleZipper a -> CycleZipper a
shiftN n_ cz_ = shiftN_mod (mod n_ (size cz_)) cz_
  where
    shiftN_mod :: Int -> CycleZipper a -> CycleZipper a
    shiftN_mod n cz = case compare n 0 of
      LT -> shiftN_mod (n+1) (shift_left cz)
      EQ -> cz
      GT -> shiftN_mod (n-1) (shift_right cz)