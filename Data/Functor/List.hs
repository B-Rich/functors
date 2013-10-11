{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Functor.List where

import           Control.Monad
import           Control.Monad.Free
import           Data.Functor.Algebra
import           Data.Functor.NatTrans
import           Data.Monoid
import           Prelude (Functor, fmap, ($), (.),
                          Bool(..), (||),
                          Int, (+),
                          error)
import qualified Prelude

cons :: (Algebra f, Monoid a) => f a -> Free f a -> Free f a
cons f (Pure a) = Free $ fmap (Pure . (<> a)) f
cons f xs       = Free $ fmap (\a -> Pure a `append` xs) f

append :: (Algebra f, Monoid a) => Free f a -> Free f a -> Free f a
append (Pure a) ys = fmap (a <>) ys
append xs (Pure a) = fmap (<> a) xs
append xs ys       = head xs `cons` (tail xs `append` ys)

instance (Algebra f, Monoid a) => Monoid (Free f a) where
    mempty  = Pure mempty
    mappend = append

head :: Algebra f => Free f a -> f a
head (Pure _) = error "Cannot take head of a Pure value"
head (Free f) = fmap go f
  where go (Pure a) = a
        go (Free m) = go (alg m)

tail :: Algebra f => Free f a -> Free f a
tail (Pure _) = error "Cannot take tail of a Pure value"
tail (Free f) = alg f

init :: Algebra f => Free f a -> Free f a
init (Pure _) = error "Cannot take init of Pair value"
init (Free f) = Free $ fmap go f
  where go (Pure a) = Pure a
        go (Free m) = case alg m of
            Free _ -> Free $ fmap go m
            x      -> x

last :: Algebra f => Free f a -> f a
last (Pure _) = error "Cannot take last of a Pure value"
last (Free f) = case alg f of
    Pure _ -> fmap go f
    x      -> last x
  where go (Pure a) = a
        go (Free m) = go (alg m)

null :: Free f a -> Bool
null (Pure _) = True
null _ = False

length :: Algebra f => Free f a -> Int
length (Pure _) = 0
length (Free f) = 1 + length (alg f)

map :: Functor g => (forall x. f x -> g x) -> Free f a -> Free g a
map _ (Pure a) = Pure a
map f (Free m) = Free $ fmap (map f) (f m)

mapN :: NatTrans f g => Free f a -> Free g a
mapN (Pure a) = Pure a
mapN (Free m) = Free $ fmap mapN (nmap m)

foldr :: Functor f
      => (forall x. f (f x) -> f x) -> (a -> f a) -> Free f a -> f a
foldr _  eta (Pure a) = eta a
foldr mu eta (Free m) = mu $ fmap (foldr mu eta) m

-- foldl :: Functor f
--       => (forall x. f (f x) -> f x) -> (a -> f a) -> Free f a -> f a
-- foldl _  eta (Pure a) = eta a
-- foldl mu eta (Free m) = fmap (foldl mu eta) m

concat :: Functor f => Free f (Free f a) -> Free f a
concat = join

any :: Algebra f => (forall x. f x -> Bool) -> Free f a -> Bool
any _ (Pure _) = False
any f (Free m) = f m || any f (alg m)
