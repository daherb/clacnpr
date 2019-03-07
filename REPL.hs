module REPL (repl) where

import Parser
import System.Console.Haskeline
import Control.Monad.State

repl =
  runInputT defaultSettings (evalStateT loop [])
  where
    loop :: StateT [Float] (InputT IO) ()
    loop =
      do
        l <- lift $ getInputLine ">>> "
        case l of
          Nothing -> lift $ return ()
          Just l -> do
            case parse l of
              Left e -> lift $ outputStrLn $ "Error: " ++ show e
              Right es -> do
                stack <- get
                let (str,stack') = evalExprs es stack
                lift $ outputStrLn $ str ++ "\n" ++ if (not . null) stack' then (show . head) stack'
                else ""
                put stack'
            loop
