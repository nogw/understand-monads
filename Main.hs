module Main where

import Control.Applicative
import Control.Monad

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


-- list monads
-- data List a = Nil | Cons (List a)

data List a = Nil | Cons a (List a) deriving Show

join' :: List (List a) -> List a
join' Nil = Nil
join' (Cons xs xss) =  cat xs (join' xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

l1 = Cons 1 (Cons 2 Nil) -- [1, 2]
l2 = Cons 3 Nil -- [3]

-- state monads (turnstile example)

data TurnstileState = Locked | Unlocked 
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)

monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
      (a6, s6) = push s5
  in ([a1, a2, a3, a4, a5, a6], s6)

  -- ([Thank,Open,Tut,Thank,Open],Locked)

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

  print $ join' $ Cons l1 (Cons l2 Nil) -- [1, 2, 3]

-- State monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))

instance Applicative (State s) where
  pure a = State (\s -> (a,s))
  (<*>) (State sa) (State sb) =
    State (\s0 -> let (fn, s1) = sa s0
                      (a,  s2) = sb s1
                  in (fn a, s2))

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State action >>= k = State $ \s -> 
    let (a, s') = action s
    in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

eval :: State s a -> s -> a
eval action = fst . runState action

exec :: State s a -> s -> s
exec action = snd . runState action