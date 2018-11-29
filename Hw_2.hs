module Hw_2(circShiftL, indices, zeroBy, triplewiseSum, revRange,
    seriesK, msort) where
import Data.List
import Data.Char
import Data.Monoid

-----------------1 Task (без рекурсии)
circShiftL :: Int -> [a] -> [a]
circShiftL n l | length l == 0 || n == 0 = l 
               | n > 0 = drop (mod n (length l)) l ++ take (mod n (length l)) l
               | n < 0 =  drop (length l - countN n l) l ++ take (length l - countN n l) l 

countN :: Int -> [a] -> Int 
countN n l = mod ((-1) * n) (length l)
--------------------------------------
--------------------------------2 Task (исправлено)

indices :: [a] -> [(Integer, a)]
indices l = zip [0..] l

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy [] _ =  []
zeroBy (x:xs) p  | p x =  x : zeroBy xs p
                 | otherwise =  mempty : zeroBy xs p
                 
triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)
-------------------------------------
-------------------------------3 Task (исправлено)
revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun 
fun (a, b) 
    | a > b = Nothing 
    | b == '\NUL' = Just (b, (succ a, b)) 
    | otherwise = Just (b, (a, pred b))

--Main Hw_2 Lib> revRange('\NUL', '\SOH')
--"\SOH\NUL"
-------------------------------------
-------------------------------4 Task (исправлено)
seriesK :: Int -> [Rational]
--seriesK k = iterate (/toRational k) 1 -- красивое решение
seriesK k = (toRational 1) : (fun k 1) 
    where fun x n = (1 /toRational (x ^ n)) : (fun x (n + 1))

--------------------------------------
-------------------------------5 Task (исправлено)
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (SortedList a) where 
    mempty = SortedList []
    
instance Ord a => Semigroup (SortedList a) where 
    (SortedList xs) <> (SortedList ys) = SortedList (mergeSortedLists xs ys)
        where   mergeSortedLists xs [] = xs
                mergeSortedLists [] ys = ys
                mergeSortedLists (x:xs) (y:ys) = 
                    if x <= y 
                       then x : mergeSortedLists xs (y:ys)
                       else y : mergeSortedLists (x:xs) ys
-------------------------------------
-------------------------------6 Task
msort :: Ord a => [a] -> SortedList a
msort [] = mempty
msort [x] = SortedList [x]--любой массив единичного размера является отсортированным
-- делим массив на примерно равные части
-- для каждой полученной пары применяем операцию слияния сортированных списков
msort xs = mappend (msort top) (msort bottom) where 
    (top, bottom) = splitAt ((length xs) `div` 2) xs 
------------------------------------
-------------------------------7 Task

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

   





