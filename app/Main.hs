module Main
  ( main,
    process,
  )
where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Hi.Evaluator            (eval)
import           Hi.Parser               (parse)
import           Hi.Pretty               (prettyValue)
import           System.Console.Haskeline

process :: MonadIO m => String -> m ()
process input = case parse input of
  Left err -> liftIO (print err)
  Right expr -> do
    result <- liftIO (eval expr)
    case result of
      Left err  -> liftIO (print err)
      Right val -> liftIO (print (prettyValue val))

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input' <- getInputLine "hi> "
      case input' of
        Just ":q"  -> return ()
        Just input -> liftIO (process input) >> loop
        Nothing    -> return ()
