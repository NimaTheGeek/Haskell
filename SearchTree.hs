-- Based on Haskell - the craft of functional programming
-- section 16.7 Search tree
-- This implements a binary search tree with a minimal API
--
-- Check out http://algs4.cs.princeton.edu/32bst/ for an
-- implementation in Java with a much more extensive API

module SearchTree
 (Tree(..),   -- Nil | Node
  nil,        -- Tree a
  isNil,      -- Tree a -> Bool
  isNode,     -- Tree a -> Bool
  leftSub,    -- Tree a -> Tree a
  rightSub,   -- Tree a -> Tree a
  treeVal,    -- Tree a -> a
  insTree,    -- Ord a => a -> Tree a -> Tree a
  delete,     -- Ord a => a -> Tree a -> Tree a
  minTree,    -- Ord a => Tree a -> Maybe a
  successor,  -- Ord a => a -> Tree a -> Maybe a
  closest,    -- Int -> Tree Int -> Int
 ) where 

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq) -- not deriving show because I wrote my own show function
           
nil :: Tree a
nil = Nil

insTree :: Ord a => a -> Tree a -> Tree a

insTree val Nil = (Node val Nil Nil)
insTree val (Node v t1 t2)
  | v == val  = Node v t1 t2
  | (val > v) = Node v t1 (insTree val t2)
  | (val < v) = Node v (insTree val t1) t2

delete :: Ord a => a -> Tree a -> Tree a

delete val Nil = Nil
delete val (Node v t1 t2)
  | (val > v) = Node v t1 (delete val t2)
  | (val < v) = Node v (delete val t1) t2
  | isNil t2  = t1
  | isNil t1  = t2
  | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree Nil   = Nothing
minTree (Node v t1 _)
  | isNil t1  = Just v
  | otherwise = minTree t1

join :: (Ord a) => Tree a -> Tree a -> Tree a
join t1 t2 =
  Node mini t1 newt
    where
      (Just mini) = minTree t2
      newt        = delete mini t2

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree a -> Bool
isNode Nil          = False
isNode (Node _ _ _) = True

leftSub :: Tree a -> Tree a
leftSub Nil           = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil           = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil          = error "treeVal"
treeVal (Node v _ _) = v

-- add your solutions here
--successor :: Ord a => a -> Tree a -> Maybe a
--successor val Nil = error "No Value!"
--successor val (Node root l r)  | val > root = minTree r | val < root = error "22222222"|otherwise = error "333333"
   
--leftSmallest :: Tree a -> Maybe a
--leftSmallest Nil = error "empty Tree"
--leftSmallest (Node root l r)
    

--rightSmallest :: Tree a -> Maybe a
--rightSmallest Nil = error "empty Tree"


successor :: Ord a => a -> Tree a -> Maybe a
successor val (Node v t1 t2)
  | ((val < v) && (isNil t1)) == True = Just v
  | ((val < v) && (isNode t1) && ((treeVal t1) <= val) && (isNode (Node (treeVal t1) t1 t2))) == True = successor val (leftSub (Node v t1 t2))
  | ((val < v) && (isNode t1) && ((treeVal t1) < val)) == True = Just v
  | ((val < v) && (isNode t1)) ==  True = successor val (leftSub (Node v t1 t2))
  | ((val > v) && (isNode t2)) == True = successor val (rightSub (Node v t1 t2))
  | ((val > v)) == True = Just v
  | ((val == v) && (isNode t2)) == True = successor val (rightSub (Node v t1 t2))
  | otherwise = Nothing

closest :: Int -> Tree Int -> Int
closest value tree
    | isNil tree == True = -1
    | ((treeVal tree) < value) ==  True = (closest'((treeVal tree) - value)  (closest value (rightSub tree ))  ) + value
    | otherwise = (closest' ((treeVal tree) - value)  (closest value (leftSub tree)) ) + value

closest' :: Int -> Int -> Int
closest' x y
    | (abs x > abs y) == True = y
    |otherwise = x


-- code to display trees

pairEntriesWithDepth :: Tree a -> Int -> [(Maybe a, Int)]

pairEntriesWithDepth Nil depth                 = [(Nothing, depth)]
pairEntriesWithDepth (Node x left right) depth =
  (Just x,depth):(pairEntriesWithDepth left (depth + 1) ++ pairEntriesWithDepth right (depth + 1))

instance (Show a) => Show (Tree a) where
  show tree = init $ unlines [replicate d '.' ++ (show' n) | (n,d) <- pairEntriesWithDepth tree 0]
    where
      show' Nothing   = "nil"
      show' (Just x)  = show x  
 
 