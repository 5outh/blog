import Control.Applicative
import Data.Monoid

import Prelude hiding (Maybe(..), Either(..))

-- Identity
data Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = Identity
  (Identity a) >>= f = f a

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  Identity a `mappend` Identity b = Identity (a `mappend` b)


-- Maybe
data Maybe = Just a | Nothing
  deriving (Show, Eq)

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just b = Just (f b)

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  Just a >>= f = f a

-- but that's not all!
-- Simple types generate powerful monads/applicatives/functors

-- Error handling!
data Either a b = Left a | Right b

-- Non-determinism!
-- data [a] = a:[a] : []

-- Polymorphic logging!
newtype Writer w a = Writer{ runWriter :: (a, w) }

-- Pure State updates!
newtype State s a = State{ runState :: a -> (s, a) }

-- Recursive-descent parsers!
newtype Parser a = Parser{ runParser :: String -> [(a, String)] } 

-- Continuations ("Complete me later" behavior)!
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- Probability Distributions / Weighted Randomness!
newtype Probability a = Probability { runProbability :: [(a,Rational)] }

-- When combined with 'do' notation, using all of these things become 
-- not only useful, but glaringly readable and a pleasure to write.