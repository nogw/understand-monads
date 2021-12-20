module Main where

-- instance Monad Maybe where  
--     return x = Just x  
--     Nothing >>= f = Nothing  
--     Just x >>= f  = f x  
--     fail _ = Nothing  

rev :: [a] -> [a] 
rev [] = []
rev xs = last xs : rev (init xs)

safeTail :: [a] -> Maybe [a]
safeTail (x : xs) = Just xs
safeTail []       = Nothing

map' :: (a -> b) -> [a] -> [b] -> Maybe [b]
map' f (x : xs) acc = map' f xs (f x : acc)  
map' f [] []        = Nothing 
map' f [] acc       = Just (rev acc) 

getFirstEvenSafe :: (Integral a) => [a] -> Maybe a
getFirstEvenSafe (x : xs) = if even x then Just x else getFirstEvenSafe xs
getFirstEvenSafe []       = Nothing

-- instance Monad [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     fail _ = []  

-- [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')] 

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
  n <- [1,2]  
  ch <- ['a','b']  
  return (n,ch)  

-- sorry, blasphemy
-- (|>) :: a -> (a -> b) -> b
-- (|>) x f = f x

-- The first monad law states that if we take a value, 
-- put it in a default context with return and then feed 
-- it to a function by using >>=, it's the same as just 
-- taking the value and applying the function to it. To put it formally:

-- return x >>= f is the same damn thing as f x

-- The second law states that if we have a monadic value 
-- and we use >>= to feed it to return, the result is 
-- our original monadic value. Formally:

-- m >>= return is no different than just m

-- The final monad law says that when we have a chain of monadic function 
-- applications with >>=, it shouldn't matter how they're nested. Formally written:

-- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)

main :: IO ()
main = do
  let safe_tail_result = safeTail ([1, 2, 3])
  print safe_tail_result
  
  let unsafe_map_result = fmap (+1) [1, 2, 3]
  print unsafe_map_result 

  let safe_map_result = map' (\x -> x * x) [1, 2, 3] []
  print safe_map_result

  -- -- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  
  -- -- (>>) :: (Monad m) => m a -> m b -> m b

  let safe_map_and_get_even_result = map' (\x -> x * x) [1, 2, 3] [] >>= getFirstEvenSafe
  print safe_map_and_get_even_result