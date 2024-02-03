{-# OPTIONS -Wtype-defaults #-}
{-# OPTIONS -Wunused-imports #-}

module Hi.Pretty
  ( prettyValue,
  )
where

import           Data.Foldable                 (toList)
import           Data.List                     (intersperse)
import           Data.Ratio                    (denominator, numerator)
import           Data.Sequence                 (Seq)
import           Hi.Base                      (HiFun (..), HiValue (..))
import           Hi.Parser                    ()
import           Hi.Utils
import           Prettyprinter                 (Doc, Pretty (pretty), hcat,
                                                viaShow)
import           Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueList list)                    = pretty "[" <> prettyList list <> pretty "]"
prettyValue (HiValueFunction HiFunList)           = pretty "list"
prettyValue (HiValueFunction HiFunRange)          = pretty "range"
prettyValue (HiValueFunction HiFunFold)           = pretty "fold"
prettyValue (HiValueFunction HiFunLength)         = pretty "length"
prettyValue (HiValueFunction HiFunToUpper)        = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower)        = pretty "to-lower"
prettyValue (HiValueFunction HiFunReverse)        = pretty "reverse"
prettyValue (HiValueFunction HiFunTrim)           = pretty "trim"
prettyValue (HiValueFunction HiFunNot)            = pretty "not"
prettyValue (HiValueFunction HiFunAnd)            = pretty "and"
prettyValue (HiValueFunction HiFunOr)             = pretty "or"
prettyValue (HiValueFunction HiFunLessThan)       = pretty "less-than"
prettyValue (HiValueFunction HiFunGreaterThan)    = pretty "greater-than"
prettyValue (HiValueFunction HiFunEquals)         = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan)    = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals)      = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf)             = pretty "if"
prettyValue (HiValueString s)                     = viaShow s
prettyValue (HiValueNull)                         = pretty "null"
prettyValue (HiValueBool True)                    = pretty "true"
prettyValue (HiValueBool False)                   = pretty "false"
prettyValue (HiValueFunction HiFunDiv)            = pretty "div"
prettyValue (HiValueFunction HiFunMul)            = pretty "mul"
prettyValue (HiValueFunction HiFunAdd)            = pretty "add"
prettyValue (HiValueFunction HiFunSub)            = pretty "sub"
prettyValue (HiValueNumber n) = case (numerator n, denominator n) of
  (p, 1) -> pretty p
  (p, q) ->
    if isInfiniteFraction n then pretty (prettyFraction p q)
    else pretty (show (fromIntegral p / fromIntegral q :: Double))

{-
  1°. (isInfiniteFraction == true && |p| < |q|)
          <=> (sign p) ++ (p) ++ ("/") ++ (q)

  2°. (isInfiniteFraction == true && |p| >= |q|)
          <=> (sign p) ++ [p/q] ++ (sign p) ++ (p - [p/q] * q) ++ ("/") ++ (q)
-}
prettyFraction :: Integer -> Integer -> String
prettyFraction p q = do
  if abs p < abs q then (if p > 0 then "" else "-") ++ show (abs p) ++ "/" <> show q
  else (if p > 0 then "" else "-") <> show (abs p `div` q) <> (if p > 0 then " + " else " - ") <> prettyFraction (abs p - (abs p `div` q) * q) q

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList list = hcat (intersperse (pretty ", ") (toList $ fmap prettyValue list))
