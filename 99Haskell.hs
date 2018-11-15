module Practice where

myLast [a] = a
myLast [] = error "nothing"
myLast (x:xs) = myLast xs



lastSecond [a] = error "only one"
lastSecond [] = error "nothin"
lastSecond [s,d] = s
lastSecond (x:y:ys) = lastSecond (y:ys)


elementAt:: [a] -> Integer  -> a
elementAt (x:xs) 1 = x
elementAt [] k = error "emptr list"
elementAt (x:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [a] = 1;
myLength [] = 0;
myLength (x:xs) = 1 + myLength(xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++  [x]


getLast :: [a] -> [a]
getLast [] = error "empty list"
getLast [s] = [s]
getLast (x:xs) = getLast xs

takeLastOut :: [a] -> [a]
takeLastOut [b] = []
takeLastOut (x:xs) = [x] ++ takeLastOut xs


--0.6--
isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs)
  | [x] == getLast (x:xs) = isPalindrome (takeLastOut xs)
  | otherwise = False

--0.8--
compress ::(Eq a)=> [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:y:ys) 
  | x == y = compress (y:ys)
  | otherwise = x : (compress (y:ys))

--1.4--
dupli::[a]->[a]
dupli [] = []
dupli [a] = [a,a]
dupli (x:xs) = x:x:(dupli(xs))

--1.5--
repli::[a]->Int->[a]
repli [] n = error "empty list"
repli (x) 0 = []
repli [a] b = a : (repli [a] (b-1))
repli (x:xy) n = (repli [x] n) ++ (repli (xy) n)








  
