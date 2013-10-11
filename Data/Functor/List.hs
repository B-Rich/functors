module Data.Functor.List where

import Control.Monad.Free
import Data.Functor.Algebra
import Prelude (Functor, fmap, error, ($))
import qualified Prelude

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
