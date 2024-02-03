module Hi.Utils
  ( isBinaryFunction,
    isLogicalFunction,
    isInfiniteFraction,
    isStringFunction,
    slice,
    toInt,
    getByIndexOrNull,
    sqParens,
    isListFunction,
    fromHiValueNumberToRational,
    skipSpace,
    lexeme,
    symbol,
    parens,
    table,
    Parser,
  )
where

import           Control.Monad.Combinators.Expr (Operator (..))
import           Data.Ratio                     (numerator)
import           Data.Scientific                (fromRationalRepetendUnlimited)
import qualified Data.Text                      as T
import           Data.Void                      (Void)
import           Hi.Base
import           Text.Megaparsec                (Parsec, between, notFollowedBy,
                                                 try)
import           Text.Megaparsec.Char           (space, space1)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

data InfixType = RightAssoc | LeftAssoc | LeftAssocSafe | NeutralAssoc deriving (Eq)

---- EVAL UTILS ----
isBinaryFunction :: HiFun -> Bool
isBinaryFunction =
  ( `elem`
      [ HiFunAdd,
        HiFunSub,
        HiFunMul,
        HiFunDiv,
        HiFunLessThan,
        HiFunNotLessThan,
        HiFunGreaterThan,
        HiFunNotGreaterThan,
        HiFunEquals,
        HiFunNotEquals
      ]
  )

isLogicalFunction :: HiFun -> Bool
isLogicalFunction = (`elem` [HiFunIf, HiFunAnd, HiFunOr])

isStringFunction :: HiFun -> Bool
isStringFunction = (`elem` [HiFunLength, HiFunToUpper, HiFunToLower, HiFunReverse, HiFunTrim])

isListFunction :: HiFun -> Bool
isListFunction = (`elem` [HiFunList, HiFunRange, HiFunFold])

isInfiniteFraction :: Rational -> Bool
isInfiniteFraction n = case fromRationalRepetendUnlimited n of
  (_, Nothing) -> False
  (_, Just _)  -> True

slice :: Int -> Int -> T.Text -> T.Text
slice a b text =
  if a >= 0 && b >= 0 then (T.take (b - a)) . (T.drop a) $ text
  else if a < 0 && b >= 0 then (T.take (b - (T.length text + a))) . (T.drop (T.length text + a)) $ text
  else if a >= 0 && b < 0 then (T.take (T.length text + b - a)) . (T.drop a) $ text
  else (T.take ((T.length text + b) - (T.length text + a))) . (T.drop (T.length text + a)) $ text

toInt :: Rational -> Int
toInt a = fromIntegral (numerator a)

getByIndexOrNull :: Rational -> T.Text -> HiValue
getByIndexOrNull ind str =
  if ind >= 0 && ind < toRational (T.length str) then (HiValueString (T.singleton (T.index str (toInt ind))))
  else (HiValueNull)

fromHiValueNumberToRational :: HiValue -> Rational
fromHiValueNumberToRational (HiValueNumber number) = number
fromHiValueNumberToRational _                      = error "HiErrorInvalidArgument"
---- EVAL UTILS ----

---- PARSER UTILS ----
skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment ";;")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sqParens :: Parser a -> Parser a
sqParens = between (symbol "[") (symbol "]")

table :: [[Operator Parser HiExpr]]
table =
  [ [commonOp "*" HiFunMul LeftAssoc, commonOp "/" HiFunDiv LeftAssocSafe],
    [commonOp "+" HiFunAdd LeftAssoc, commonOp "-" HiFunSub LeftAssoc],

    [ commonOp "<=" HiFunNotGreaterThan NeutralAssoc, commonOp "<" HiFunLessThan NeutralAssoc,
      commonOp ">=" HiFunNotLessThan NeutralAssoc, commonOp ">" HiFunGreaterThan NeutralAssoc,
      commonOp "/=" HiFunNotEquals NeutralAssoc, commonOp "==" HiFunEquals NeutralAssoc
    ],

    [commonOp "&&" HiFunAnd RightAssoc],
    [commonOp "||" HiFunOr RightAssoc]
  ]
  where
    commonOp :: String -> HiFun -> InfixType -> Operator Parser HiExpr
    commonOp symb f infixType =
      let assoc = case infixType of
            RightAssoc    -> InfixR
            LeftAssoc     -> InfixL
            LeftAssocSafe -> InfixL . try
            NeutralAssoc  -> InfixN
          options = case infixType of
            LeftAssocSafe -> symbol symb <* notFollowedBy (symbol "=")
            _             -> symbol symb
       in assoc $ (\a b -> HiExprApply (HiExprValue (HiValueFunction f)) [a, b]) <$ options
---- PARSER UTILS ----
