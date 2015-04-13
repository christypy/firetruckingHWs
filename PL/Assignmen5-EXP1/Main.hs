---   Main.hs
----------------------------------------------------------------
-- Main module for the EXP1 interpreter

module Main where

--import System.Environment   -- to obtain command-line arguments

import Err (Error(..))
import Eval (evalExp)
import Parser (parseFile)

-- Take a file path in the form of astring, and try to parse
-- the contents of the file into abstract syntax. If this
-- succeeds, evaluate the abstract syntax, and display the
-- result.

parseEval :: String -> IO ()
parseEval fname =
  do { r <- parseFile fname
     ; case r of              
       Left err -> do {putStr "parse error: "; print err}
       Right e ->
           case (evalExp e) of
                Error msg ->
                  do {putStr "evaluation error: "; print msg}
                S v -> putStr $ show v
     }

