module Data.Functor.Algebra where

import Control.Monad.Free

class Functor f => Algebra f where
    alg :: f a -> a

instance Algebra ((,) e) where
    alg (_, a) = a

instance Algebra f => Algebra (Free f) where
    alg (Pure a) = a
    alg (Free f) = alg (alg f)
