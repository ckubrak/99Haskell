import Data.List

myLast :: [a] -> a
myLast = (head . reverse)

myButLast :: [a] -> a
myButLast = (myLast . init)

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i-1)

myLength :: [a] -> Int
myLength = foldl (const . (+1)) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse (xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

compress :: (Eq a) => [a] -> [a]
compress = map head . group

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\x -> (length x, head x)) (group xs)
