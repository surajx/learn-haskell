module Crashcourse where

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n-1)

mult :: Int -> Int -> Int
mult x y = x * y

multTwo :: Int -> Int
multTwo = mult 2

item :: (String, Float)
item = ("cola", 3.14)

addPair :: (Int, Int) -> Int
addPair (x,y) = x + y

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

len :: [a] -> Int
len = len' 0
len' :: Int -> [a] -> Int
len' acc [] = acc
len' acc (_:xs) = len' (acc+1) xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x:filter' f xs
    | otherwise = filter' f xs 

data Shape = Circle Float | Rectangle Float Float deriving (Eq, Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

data List a = Nil | Cons a (List a) deriving Show

items :: List Int
items = Cons 3 (Cons 4 (Cons 5 Nil))

data Tree a = Null | Node a (Tree a) (Tree a) deriving Show
mTree :: Tree Int
mTree = Node 23 (Node 13
                (Node 5
                    (Node 6 Null Null)
                    Null
                )
                (Node 16 Null Null)
            )
            (Node 42
                (Node 31 Null Null)
                (Node 68 Null Null)
            )

smallest :: Tree Int -> Int
smallest Null = -1
smallest (Node x t1 t2) = smallest' (smallest' x t1) t2

smallest' :: Int -> Tree Int -> Int
smallest' m Null = m
smallest' m (Node x t1 t2) = min m (smallest' (smallest' x t1) t2)

largest :: Tree Int -> Int
largest Null = -1
largest (Node x t1 t2) = largest' (largest' x t1) t2

largest' :: Int -> Tree Int -> Int
largest' l Null = l
largest' l (Node x t1 t2) = max l (largest' (largest' x t1) t2)

numNodes :: Tree a -> Int
numNodes Null = 0
numNodes (Node _  t1 t2) = 1 + numNodes t1 + numNodes t2

treeDepth :: Tree a -> Int
treeDepth Null = -1
treeDepth (Node _ t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)

revTree :: Tree a -> Tree a
revTree Null = Null
revTree (Node x t1 t2) = Node x (revTree t2) (revTree t1)

searchTree :: Tree Int -> Int -> Bool
searchTree Null _ = False
searchTree (Node x t1 t2) a = a==x || searchTree t1 a || searchTree t2 a

insertTree :: Tree Int -> Int -> Tree Int
insertTree Null e = Node e Null Null
insertTree (Node x t1 t2) e = if e<=x then 
    Node x (insertTree t1 e) t2 else Node x t2 (insertTree t2 e)

toList :: Tree a -> [a]
toList Null = []
toList (Node x t1 t2) = x:toList t1 ++ toList t2

cntPredicateMatch:: Tree a -> (a -> Bool) -> Int
cntPredicateMatch Null _ = 0
cntPredicateMatch (Node x t1 t2) f = if f x then
    1 + cntPredicateMatch t1 f + cntPredicateMatch t2 f
    else cntPredicateMatch t1 f + cntPredicateMatch t2 f


