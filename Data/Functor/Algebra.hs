module Data.Functor.Algebra where

class Functor f => Algebra f where
    alg :: f a -> a

instance Algebra ((,) e) where
    alg (_, a) = a
