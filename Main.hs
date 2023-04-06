{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<|>))
import Data.SCargot (decode, setCarrier, mkParser)
import Data.SCargot.Repr (toRich, fromRich, RichSExpr (..))
import Data.SCargot.Comments (withLispComments)
import Data.SCargot.Print (basicPrint, encode, setMaxWidth)
import Data.Text (Text)
import System.Environment (getArgs)
import Text.Parsec (alphaNum, many1, oneOf)
import Text.Parsec.Text (Parser)

import qualified Data.Text as T
import qualified Data.Text.IO as T

atomParser :: Parser Text
atomParser = do
  T.pack <$> many1 (alphaNum <|> oneOf "._+-*!<>@=/")

process :: String -> Either String [RichSExpr Text]
process input = do
  decode (withLispComments parser) (T.pack input)
  where
    parser = setCarrier (return . toRich) (mkParser atomParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tgt] -> do
      input <- readFile tgt
      case process input of
        Right xs -> do
          let printer = setMaxWidth 100 (basicPrint id)
          T.putStrLn $ encode printer $ map fromRich xs
        Left err -> putStrLn err
    _ -> putStrLn "Usage: ppq <target>"
