-- Equality between ChordTree?
-- Convert from ChordTree to ChordTreeId (add identifiers: the hash of the tree)
-- convert (Twice v1 v2) =
-- 	if v1 == v2
-- 		then TwiceId hash(convert v1) (vconId (hash v2))
-- 		else TwiceId hash(convert v1, convert v2) (convert v1, convert v2)
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- import Data.Generics.Uniplate.Data (universe)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List

data Tree a = Leaf a | Node a [Tree a] | Empty
    deriving (Show, Data, Eq)

-- instance Eq a => Eq (Tree a) where
  -- (==) tree1 tree2 = treeEquals tree1 tree2

  -- checks whether or not the first tree occurs somewhere in the second
t :: Tree Int
t = Node 3 [Node 5 [Leaf 11, Leaf 13, Leaf 15], Leaf 7, Leaf 9]

t2 :: Tree Int
t2 = Node 1 [Node 5 [Leaf 11, Leaf 13, Leaf 15], Node 5 [Leaf 11, Leaf 13, Leaf 15]]

treeEquals :: Eq a => Tree a -> Tree a -> Bool
treeEquals (Leaf n1) (Leaf n2) = n1 == n2
treeEquals (Node n1 xs1) (Node n2 xs2) = n1 == n2 && listTreeEquals xs1 xs2
    where
    listTreeEquals [] [] = True
    listTreeEquals (x1 : xs1) (x2 : xs2) = treeEquals x1 x2 && listTreeEquals xs1 xs2
    listTreeEquals _ _ = False
treeEquals _ _ = False

-- allSubtrees :: Data a => Tree a -> [Tree a]
-- allSubtrees = universe

getSubtrees :: Tree a -> [Tree a]
getSubtrees (Leaf x)    = [Leaf x]
getSubtrees (Node x xs) = Node x xs : concat (map getSubtrees xs)

isSubtree :: (Eq (Tree a), Data a)=> Tree a -> Tree a -> Bool
isSubtree x y = elem y (getSubtrees x)

listhasdup :: Eq a => [a] -> Bool
listhasdup x =  length x == length (nub x)

--   -- checks for every subtree whether or not it occurs elsewhere
noDuplicates :: (Eq (Tree a), Data a)=> Tree a -> Bool
noDuplicates x = listhasdup (getSubtrees x)

-- New section with CSE

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) | Shared' ID | Empty'
newtype ID = ID Int

-- subtrees t: [Tree]

removeSinleOccs :: Eq a => [a] -> [a]
removeSinleOccs [] = []
removeSinleOccs (x:xs) =
  if x `elem` xs
    then x:removeSinleOccs (filter (/= x) xs)
    else removeSinleOccs xs

-- chechIfElem :: [(Tree a, Int)] -> (Tree a, Int) -> Bool
checkIfElem l x
  |x `elem` l = True
  |otherwise = False

cseOne :: [(Tree a, ID)] -> Tree a -> Tree a
cseOne d t =
  if (checkIfElem d (t, id)) then
    shared id
  else cse d t
    where d = zip (removeSinleOccs (getsubtrees t)) ([1 ..])

cse :: [(Tree a, ID)] -> Tree a -> Tree' a
cse d (Leaf x) = cseOne d (Leaf x)
cse d (Node l r) =
  if (Node l r) `elem` d
    then that ID
    else if l `elem` d
      then cse d l
      else if r `elem` d
        then cse d r
        else Node l r


-- cse :: Tree a -> (Tree' a, Map ID Tree a)
-- cse Empty = (Empty, Empty)
-- cse (Node x xs) = (Empty, Empty)

-- Map :: ID -> Tree  (Data.map)
