module Data.Functor.NatTrans where

import Data.Functor.Algebra
import Data.Functor.Identity
import Prelude (Functor, fmap, error, ($))
import qualified Prelude

class (Functor f, Functor g) => NatTrans f g where
    nmap :: forall a. f a -> g a

instance Algebra f => NatTrans f Identity where
    nmap (alg -> a) = Identity a
