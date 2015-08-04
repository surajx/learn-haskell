module MyTree where
data Tree a = Null | Node a (Tree a) (Tree a) deriving Show
aTree :: Tree Float
aTree = Node 2 (Node 3 Null Null) (Node 4.5 Null Null)

bigTree :: Tree Int
bigTree = Node 23 ( Node 13 ( Node 5 Null ( Node 6 Null Null )) ( Node 16 Null Null )) ( Node 42 ( Node 31 Null Null ) ( Node 68 Null Null ))

flatten :: Tree a -> [a]
flatten Null = []
flatten (Node a t1 t2) = flatten t1 ++ [a] ++ flatten t2

sumTree :: Tree Float -> Float
sumTree Null = 0
sumTree (Node a t1 t2) = sumTree t1 + a + sumTree t2

isNull :: Tree a -> Bool
isNull Null = True
isNull (Node {}) = False

depth :: Tree a -> Int
depth Null = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

smallest :: Tree Int -> Int
smallest Null = maxBound :: Int
smallest (Node a t1 t2) = min a (min (smallest t1) (smallest t2))
