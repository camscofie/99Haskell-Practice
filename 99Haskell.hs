module Test where

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

isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs)
  | [x] == getLast (x:xs) = isPalindrome (takeLastOut xs)
  | otherwise = False


compress ::(Eq a)=> [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:y:ys) 
  | x == y = compress (y:ys)
  | otherwise = x : (compress (y:ys))











  