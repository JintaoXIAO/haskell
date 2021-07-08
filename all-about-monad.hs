{-#LANGUAGE NoImplicitPrelude#-}

import Prelude
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Control.Monad

data Sheep = Sheep (Maybe Sheep, Maybe Sheep)
           | Sheep0
           deriving (Show, Eq)

mother, father :: Sheep -> Maybe Sheep
father Sheep0 = Nothing
father (Sheep p) = fst p

mother Sheep0 = Nothing
mother (Sheep p) = snd p

-- father . mother $ s
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = case (mother s) of
                          Nothing -> Nothing
                          Just ms -> father ms

-- father . father . mother $ s
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = case (mother s) of
                                 Nothing -> Nothing
                                 Just ms -> case (father ms) of
                                   Nothing -> Nothing
                                   Just fms -> father fms

comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

mothersPaternalGrandfather1 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather1 s = (Just s) `comb` mother `comb` father `comb` father

maternalGrandfather1 :: Sheep -> Maybe Sheep
maternalGrandfather1 s = (return s) >>= mother >>= father

fatherMaternalGrandmother :: Sheep -> Maybe Sheep
fatherMaternalGrandmother s = (return s) >>= father >>= mother >>= father

mothersPaternalGrandfather2 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather2 s = do m <- mother s
                                   fm <- father m
                                   father fm

mothersPaternalGrandfather3 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather3 s =
  mother s >>= \m ->
  father m >>= \fm ->
  father fm

maternalGrandfather2 :: Sheep -> Maybe Sheep
maternalGrandfather2 s = mother s >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = father s >>= mother >>= father

mothersPaternalGrandfather4 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather4 s = mother s >>= father >>= father

parent :: Sheep -> Maybe Sheep
parent s = (father s) `mplus` (mother s)

grandparent :: Sheep -> Maybe Sheep
grandparent s = father s >>= parent

parent1 :: Sheep -> [Sheep]
parent1 s = maybeToList (father s) `mplus` maybeToList (mother s)

-- mapM :: (Tranversable t, Monad m) => (a -> m b) -> t a -> m (t b)
grandparent1 :: Sheep -> [Sheep]
grandparent1 s = foldl (++) [] $ mapM parent1 $ maybeToList (father s)

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
traceFamily :: Sheep -> [( Sheep -> Maybe Sheep )] -> Maybe Sheep
traceFamily s l = foldM getParent s l where
  getParent s f = f s

mothersPaternalGrandfather5 s = traceFamily s [mother, father, father]
paternalGrandmother s = traceFamily s [father, mother]

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x

