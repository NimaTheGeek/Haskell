-- based on 16.8 Set of
-- Haskell - the craft of functional programming

module Set
  (
    Set       , 
    empty     , -- Set a
    sing      , -- a -> Set a 
    memSet    , -- Ord a => Set a -> a -> Bool
    union     , -- Ord a => Set a -> Set a -> Set a
    inter     , -- Ord a => Set a -> Set a -> Set a
    diff      , -- Ord a => Set a -> Set a -> Set a
    symDiff   , -- Ord a => Set a -> Set a -> Set a
    makeSet   , -- Ord a => [a] -> Set a
    mapSet    , -- Ord b => (a -> b) -> Set a -> Set b
    foldSet   , -- (a -> a -> a) -> Set a -> Set a
    filterSet , -- (a -> Bool) -> Set a -> Set a
    powerSet  , -- Set a => Set (Set a)
    card      , -- Set a -> Int
  ) where

import Data.List hiding (union)

instance Eq a => Eq (Set a) where
  (==) = eqSet
instance Ord a => Ord (Set a) where
  (<=) = leqSet
instance Show a => Show (Set a) where
  show (Set xs) = show xs

newtype Set a = Set [a]

empty :: Set a
empty = Set []

sing :: a -> Set a
sing x = Set [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set [])     y = False
memSet (Set (x:xs)) y
  | x <  y    = memSet (Set xs) y
  | x == y    = True
  | otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys)
  | x <  y    = x : uni xs (y:ys)
  | x == y    = x : uni xs ys
  | otherwise = y : uni (x:xs) ys

inter :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys)
  | x <  y    = int xs (y:ys)
  | x == y    = x : int xs ys
  | otherwise = int (x:xs) ys
    
subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys)
  | x <  y    = False
  | x == y    = subS xs ys
  | otherwise = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
  where
    remDups []  = []
    remDups [x] = [x]
    remDups (x:y:xs)
      | x < y     = x : remDups (y:xs)
      | otherwise = remDups (y:xs)
        
mapSet :: (Ord b) => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set xs) = (foldr f x xs)

card :: Set a -> Int
card (Set xs) = length xs


-- put your solutions here

diff :: (Ord a) => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (diff' xs ys)

diff' :: Ord a => [a] -> [a] -> [a]
diff' [] ys = ys
diff' xs [] = xs
diff' xs ys =
    let a = filter (`elem` xs) ys
        b = filter (`notElem` a) xs
    in  b 

symDiff :: (Ord a) => Set a -> Set a -> Set a
symDiff (Set xs) (Set ys) = Set (diff' (uni xs ys) (int xs ys))


powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet (Set xs) = makeSet (listToSet(powerSet' xs))

listToSet :: [[a]] -> [Set a]
listToSet [] = []
listToSet x = [Set (head x)] ++ listToSet (tail x)

powerSet' :: (Ord a) => [a] -> [[a]]
powerSet' [] = [[]]
powerSet' (x:xs) = powerSet' xs ++ map (x:) (powerSet' xs)

