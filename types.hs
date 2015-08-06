module MyTypes where
data Voter = MkVoter String String
aVoter :: Voter
aVoter = MkVoter "Alan Turing" "Milton Keynes, UK"

--Box2D is the Type constructor
--Square and Rectangle are the Data constructor
data Box2D = Square Float | Rectangle Float Float

boxArea :: Box2D -> Float
boxArea (Square a) = a*a
boxArea (Rectangle a b) = a*b

--Here Cons is the Data Constructor for type IntList, it takes in
--an Int and a IntList and returns an IntList type.
--data IntList = Null | Cons Int IntList

--Here Cons is the Data Constructor for type List, it takes in
--any type 'a' and a List of type a and returns a List type.
data List a = Null | Cons a (List a) deriving Show

aList :: List Int
aList = Cons 2 Null

someList :: (Integral a) => List a
someList = Cons 2 (Cons 3 (Cons 5 Null))
