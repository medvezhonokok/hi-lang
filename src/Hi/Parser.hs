module Hi.Parser (parse) where

import           Control.Monad.Combinators      (choice, manyTill, sepBy, (<|>))
import           Control.Monad.Combinators.Expr (makeExprParser)
import           Data.Sequence                  (fromList)
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)
import           Hi.Base
import qualified Hi.Utils                      as Utils
import           Text.Megaparsec                (eof, runParser)
import           Text.Megaparsec.Char           (char, string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (ParseErrorBundle)

---- COMBINATORS ----
pNumber :: Utils.Parser Rational
pNumber = toRational <$> Utils.lexeme (L.signed Utils.skipSpace L.scientific)

pBool :: Utils.Parser Bool
pBool = (string "true" >> return True) <|> (string "false" >> return False)

pNull :: Utils.Parser HiValue
pNull = (string "null" >> return HiValueNull)

pStringLiteral :: Utils.Parser Text
pStringLiteral = do
  collector <- char '"' >> manyTill L.charLiteral (char '"')
  return (pack collector)

pFunction :: Utils.Parser HiFun
pFunction = choice
  [
    HiFunAdd            <$ Utils.symbol "add",
    HiFunSub            <$ Utils.symbol "sub",
    HiFunMul            <$ Utils.symbol "mul",
    HiFunDiv            <$ Utils.symbol "div",

    HiFunLessThan       <$ Utils.symbol "less-than",
    HiFunNotLessThan    <$ Utils.symbol "not-less-than",
    HiFunGreaterThan    <$ Utils.symbol "greater-than",
    HiFunNotGreaterThan <$ Utils.symbol "not-greater-than",
    HiFunEquals         <$ Utils.symbol "equals",
    HiFunNotEquals      <$ Utils.symbol "not-equals",

    HiFunAnd            <$ Utils.symbol "and",
    HiFunOr             <$ Utils.symbol "or",
    HiFunIf             <$ Utils.symbol "if",
    HiFunNot            <$ Utils.symbol "not",

    HiFunLength            <$ Utils.symbol "length",
    HiFunToUpper           <$ Utils.symbol "to-upper",
    HiFunToLower           <$ Utils.symbol "to-lower",
    HiFunReverse           <$ Utils.symbol "reverse",
    HiFunTrim              <$ Utils.symbol "trim",

    HiFunList              <$  Utils.symbol "list",
    HiFunRange             <$  Utils.symbol "range",
    HiFunFold              <$  Utils.symbol "fold"
  ]

pArguments :: Utils.Parser [HiExpr]
pArguments = (Utils.parens $ parse' `sepBy` (Utils.symbol ","))

pList :: Utils.Parser HiValue
pList = HiValueList . fromList <$> (Utils.sqParens $ pHiValue `sepBy` (Utils.symbol ","))

pHiValue :: Utils.Parser HiValue
pHiValue = HiValueNumber <$> pNumber
  <|> HiValueBool <$> pBool
  <|> HiValueFunction <$> pFunction
  <|> HiValueString <$> pStringLiteral
  <|> pNull
  <|> pList
---- COMBINATORS ----

hiExprConsumer :: HiExpr -> Utils.Parser HiExpr
hiExprConsumer value = do
  function                <- pure value
  arguments               <- pArguments
  let evaluatedExpression = HiExprApply function arguments
  hiExprConsumer evaluatedExpression <|> pure evaluatedExpression

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (Utils.skipSpace *> parse' <* eof) "empty"

parse' :: Utils.Parser HiExpr
parse' = makeExprParser pHiExpr Utils.table
  where
    pHiExpr :: Utils.Parser HiExpr
    pHiExpr = do
      hiValue <- Utils.skipSpace *> pExpr <* Utils.skipSpace
      hiExprConsumer hiValue <|> pure hiValue

    pExpr :: Utils.Parser HiExpr
    pExpr = choice
      [ Utils.parens parse',
        HiExprValue <$> pHiValue
      ]
