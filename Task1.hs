--Kochneva Olga 5381

module Text1 where

import Data.Char
import Data.List

--Task 1
id_ :: a -> a
id_ x = x

eval :: (a -> b, a) -> b
eval (x, y) = x y
--исправлено
exchange :: (a, b) -> (b, a)
exchange (x, y) = (y, x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose x y z = x (y z)

curry_ :: ((a, b) -> c) -> (a -> b -> c)
curry_ x y z = x (y, z)
--исправлено
associate :: (a, (b, c)) -> ((a, b), c)
associate (x, (y, z)) = ((x, y), z)

--Task 2
minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax (x : []) = Just (x, x)
minMax (x : xs)  = Just (helper min (x : xs), helper max (x : xs)) where
                    helper f [x] = x
                    helper f (x : xs) = f x (helper f xs)

--Task 3 Stepik 1.6 /// c - count, s - sum
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n | n == 0 = (0, 1)
              | otherwise = helper ((abs n), 0) (0, 0) where
                   helper (0, i) (s, c) = (s + i, c)
                   helper (x, i) (s, c) = helper (divMod x 10) (s + i, c + 1)
{--sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n | n == 0 = (0, 1)
              | otherwise = helper (abs n) 0 (0, 0) where
                   helper 0 i (s, c) = (s + i, c)
                   helper x i (s, c) = helper (div x 10) (mod x 10) (s + i, c + 1)--}
                   

--Task 4 Boyer-Moore majority vote algorithm
--the difficulty islinear, O(n)
--Монада - класс типов, (Maybe в нашем случае)
{-- >>= - оператор монадического связывания (bind)
(>>=) :: m a -> (a -> m b) -> m b - связывает два монадических вычисления
-> -стрелка Клейсли, возвращает значение упакованное в монадический контейнер
Извлекаем значение из монады candidate, подаем его в result, (result - оборачивает в монаду значение с полученное в функции major_c,
которое является преобладающим элементом) и в итоге вернем это значение запакованное в монадический контейнер. 
--}
majority :: Eq a => [a] -> Maybe a 
majority xs = candidate xs >>= result xs where
                               result :: Eq a => [a] -> a -> Maybe a
                               result xs c | major_c c xs = Just c
                                           | otherwise = Nothing


--eliminates all but a single candidate.
candidate :: Eq a => [a] -> Maybe a
candidate [] = Nothing
candidate (x:xs) = Just (find_cand x xs)

--returns the element, that can be majority
find_cand :: Eq a => a -> [a] -> a
find_cand cand = fst . foldr step (cand, 1) where
                                                step x (can, cnt)
                                                    | can == x  = (can, cnt + 1)
                                                    | cnt == 0  = (x , 1 )
                                                    | otherwise = (can, cnt - 1)
      
--verify if the element is major
major_c :: Eq a => a -> [a] -> Bool
major_c can = (> 0) . foldr step 0 where
                                        step x cnt
                                         | can == x  = cnt + 1
                                         | otherwise = cnt - 1

--Task 5
f :: (a -> a) -> Int -> (a -> a)
f g n  | n == 1 = g . id
       | n > 0  = g . (f g (n - 1))
       | otherwise = error "n must be positive number"

--Task 6
--использование аккумулирующей функции для улучшения асимптотики

fibonacci :: Integer -> Integer
fibonacci n = mod (fibonacci' n) 10 

fibonacci' n = helper 0 1 n where
                              helper a b 0 = a
                              helper a b 1 = b
                              helper a b n = helper b (a + b)(n - 1)


{--fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0 = mod (fibonacci (n - 1) + fibonacci (n - 2)) 10--}

--Task 7
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) | x == last xs = isPalindrome $ init xs
                    | otherwise = False
