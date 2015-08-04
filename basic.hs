module Test where
double x = x+x
double2 x y = double x + double y
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
doubleSmall x = if x<100
  then x*2
  else x
doubleSmall' x = (if x<100 then x+x else x) + 1
ccat x y = x++y

oddEvenMapper xs = [if odd x then "odd" else "even" | x<-xs]
even3Mul xs = [ x | x<-xs, x `mod` 3 == 0, even x]
--length' xs = sum [1 | _<-xs]
abbreviate xs = [if x==' ' then '.' else x | x<-xs, x `elem` ['A'..'Z'] || x==' ']
abbreviate' xs = [if odd x then '.' else '_' | x<-xs, x `elem` [0..9]]

rightTriangle l = [(a,b,c) | c<-[1..l], b<-[1..c], a<-[1..b], a^2 + b^2 == c^2]
get1st (a,_,_) = a
get2nd (_,a,_) = a
get3rd (_,_,a) = a
rightTriangle' p l= [x | x<-rightTriangle l, get3rd x + get1st x + get2nd x == p]

lucky ::  Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!!"
lucky x = "hash: " ++ show x

factorial' 0 = 1
factorial' n = n * factorial (n-1)

kidWords = ["Apple","Boy","Cat"]

getWord4Alpha a words = head [x | x<-words, a `elem` ['A'..'Z'], head x==a]

addVector (x1,y1) (x2,y2) = (x1+x2, y1+y2)

head' :: [a] -> a
head' [] = error "You are a headless horseman!!"
head' (x:xs) = x

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

getBMIMsg :: Float -> Float -> String
getBMIMsg w h
  | bmi <=18.5 = "Undeweight: " ++ show bmi
  | bmi <=25   = "Normal: " ++ show bmi
  | bmi <=30   = "Obese: " ++ show bmi
  | otherwise    = "Dead: " ++ show bmi
  where bmi = w/h^2
mult :: Num a => a -> (a -> a)
mult x y = x * y
multTwo :: Int -> Int
multTwo = mult 2

