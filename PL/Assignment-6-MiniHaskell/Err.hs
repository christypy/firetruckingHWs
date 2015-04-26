-- Computer Science 320 (Fall, 2008)
-- Concepts of Programming Languages
--
-- Assignments 6, 7, and 8
--   Err.hs

----------------------------------------------------------------
-- Representation for computations with errors

module Err (Error(..), mapError)
  where
  
data Error a = S a
             | Error String
             
instance Show a => Show (Error a) where
  show (Error s) = s
  show (S c)  = show c

----------------------------------------------------------------
-- Lifted version of "cons" and "map" for "Error a".

consError :: Error a -> Error [a] -> Error [a]
consError(S x) (S xs) = S $ x:xs    -- S (x:xs)
consError (Error msg) _ = Error msg
consError _ (Error msg) = Error msg

mapError :: (a -> Error b) -> [a] -> Error [b]
mapError f (x:xs) = consError (f x) (mapError f xs)
mapError f [] = S []

--eof