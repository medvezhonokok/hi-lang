module Hi.Base
  ( HiError (..),
    HiExpr (..),
    HiFun (..),
    HiValue (..),
  )
where

import           Data.Sequence (Seq)
import           Data.Text     (Text)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  deriving (Show, Eq)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  deriving (Show, Eq)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show, Eq)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  | HiErrorEndOfInput
  | HiErrorEmpty
  deriving (Show, Eq)
