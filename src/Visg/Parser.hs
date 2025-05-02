{-# LANGUAGE OverloadedStrings #-}

module Visg.Parser where

import Data.Char (toUpper)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (
  ParseErrorBundle,
  Parsec,
  anySingle,
  eof,
  many,
  manyTill,
  runParser,
  skipMany,
  skipSome,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (char, eol, space)
import Text.Megaparsec.Char.Lexer qualified as L

type GCode = [Action]

data Action = Action {code :: String, assocs :: [(Char, Float)]}
  deriving (Show, Eq)

type Parser = Parsec Void Text

parseGCode :: Text -> Either (ParseErrorBundle Text Void) GCode
parseGCode =
  runParser (many action <* eof) ""

action :: Parser Action
action = do
  code <- codeP
  skipMany $ char ' '
  Action code <$> paramsP

codeP :: Parser String
codeP = do
  l <- anySingle
  num :: Int <- lexeme L.decimal
  pure $ l : show num

paramsP :: Parser [(Char, Float)]
paramsP = manyTill param ((skipSome eol $> ()) <|> eof)

param :: Parser (Char, Float)
param = do
  skipMany (char ' ')
  name <- anySingle
  skipMany (char ' ')
  value <- signedNumber
  pure (toUpper name, value)

symbol :: Text -> Parser Text
symbol = L.symbol space

signedNumber :: Parser Float
signedNumber = try (L.signed space (lexeme L.float)) <|> L.signed space (lexeme L.decimal)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany $ char ' ')

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) GCode)
parseFile p = parseGCode <$> TIO.readFile p
