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


--1.6--
drop ::[a]->Int->[a]
drop [] n = error "empty list"
drop (x) 0 = (x)
drop (x:xs) 1 = (xs)
drop (x:xs) n = x : (Practice.drop (xs) (n-1))


--1.7--
split::[a]->Int->([a],[a])
split [] n = error "empty list"
split (x:xs) n
  | n>0 = (x:xz, yz)
  | otherwise = ([],xs)
  where (xz,yz) = split xs (n-1)


--1.8--
slice::[a]->Int->Int->[a]
slice [] b c = error "empty list"
slice (x:xs) b c
  | c<0||b>myLength (x:xs)||c < b = error "the end point and start point has error"
  | b>0 = slice xs (b-1) (c-1)
  | b==0 && c>0 = x:(slice xs b (c-1))
  | b==0 && c==0 = [x]
  | otherwise = []











