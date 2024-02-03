module Hi.Evaluator
  ( eval,
  )
where

import           Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import           Data.Foldable        (toList)
import           Data.Ratio           (numerator)
import           Data.Semigroup       (stimes)
import           Data.Sequence        (fromList, (><), Seq, drop, take, index)
import           Data.Text            (singleton)
import qualified Data.Text            as T
import           Hi.Base             (HiError (..), HiExpr (..), HiFun (..),
                                       HiValue (..))
import           Hi.Utils

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

eval' :: Monad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue value) = pure value
eval' (HiExprApply function arguments) = do
  pureValue <- eval' function
  case pureValue of
    (HiValueFunction hiFunction) -> evalFunction hiFunction arguments
    (HiValueString hiString)     -> evalStringWithArguments hiString arguments
    (HiValueList hiList)         -> evalListWithArguments hiList arguments
    _                            -> throwError HiErrorInvalidFunction

evalFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFunction function arguments
  | isBinaryFunction function  = evalBinaryFunction function arguments
  | isLogicalFunction function = evalLogicalFunction function arguments
  | isStringFunction function  = evalStringOrListUnaryFunction function arguments
  | isListFunction function    = evalListFunction function arguments
  | otherwise                  = evalNotFunction function arguments
  where
    evalNotFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
    evalNotFunction function' arguments'
      | length arguments' /= 1 = throwError HiErrorArityMismatch
      | otherwise = do
        a <- ExceptT (eval (head arguments'))
        case a of
          HiValueBool a' -> if function' == HiFunNot then pure (HiValueBool (not a'))
                            else throwError HiErrorInvalidFunction
          _              -> throwError HiErrorInvalidArgument

evalListFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalListFunction function arguments = do
  case function of
    HiFunList -> do
       evaluatedArgs <- traverse eval' arguments
       pure (HiValueList (fromList evaluatedArgs))
    HiFunRange -> evalRangeList arguments
    HiFunFold  -> evalFoldList arguments
    _       -> throwError HiErrorInvalidFunction

evalRangeList :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalRangeList arguments = do
   if length arguments /= 2 then throwError HiErrorArityMismatch
   else do
     from <- eval' (head arguments)
     to <- eval' (arguments !! 1)
     case (from, to) of
         (HiValueNumber from', HiValueNumber to') ->
           if from' <= to' then pure (HiValueList (fromList (map HiValueNumber [from'..to'])))
           else pure (HiValueList (fromList []))
         _ -> throwError HiErrorInvalidArgument

evalFoldList :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalFoldList [f, expr] = do
  pureFunction <- ExceptT (eval f)
  pureArgumentsExpression <- eval' expr
  pureList <- case pureArgumentsExpression of
    HiValueList list -> pure list
    _                -> throwError HiErrorInvalidArgument
  let rationalList = map fromHiValueNumberToRational (toList pureList)
  case pureFunction of
    HiValueFunction HiFunAdd -> pure $ HiValueNumber $ foldl (+) 0 rationalList
    HiValueFunction HiFunSub -> pure $ HiValueNumber $ foldl (-) 0 rationalList
    HiValueFunction HiFunMul -> pure $ HiValueNumber $ foldl (*) 1 rationalList
    HiValueFunction HiFunDiv -> do
      case rationalList of
        []       -> throwError HiErrorArityMismatch
        (x : xs) -> pure $ HiValueNumber $ foldl (/) x xs
    _ -> throwError HiErrorInvalidFunction
evalFoldList _ = throwError HiErrorInvalidArgument

evalListWithArguments ::  Monad m => (Seq HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
evalListWithArguments hiList arguments =
  case length arguments of
    0 -> throwError HiErrorArityMismatch
    1 -> do
      i <- ExceptT (eval (head arguments))
      case (i, hiList) of
        (HiValueNumber i', _) -> do
          let ind = toInt i'
          if ind < length hiList && ind >= 0
            then pure (Data.Sequence.index hiList ind)
            else throwError HiErrorInvalidArgument
        _ -> throwError HiErrorInvalidArgument
    2 -> do
      from <- ExceptT (eval (head arguments))
      to <- ExceptT (eval (arguments !! 1))
      case (from, to, hiList) of
        (HiValueNumber from', HiValueNumber to', _) ->
          pure (HiValueList (Data.Sequence.drop (toInt from') (Data.Sequence.take (toInt to') hiList)))
        (HiValueNumber from', HiValueNull, _) ->
          pure (HiValueList (Data.Sequence.drop (toInt from') hiList))
        (HiValueNull, HiValueNumber to', _) ->
          pure (HiValueList (Data.Sequence.take (toInt to') hiList))
        _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorArityMismatch

evalStringWithArguments :: Monad m => T.Text -> [HiExpr] -> ExceptT HiError m HiValue
evalStringWithArguments hiString arguments =
  case (length arguments) of
    0 -> throwError HiErrorArityMismatch
    1 -> do
      i <- ExceptT (eval (head arguments))
      case (i, hiString) of
        (HiValueNumber i', _ ) -> pure (getByIndexOrNull i' hiString)
        _                      -> throwError HiErrorInvalidArgument
    2 -> do
      from <- ExceptT (eval (head arguments))
      to   <- ExceptT (eval (arguments !! 1))
      case (from, to, hiString) of
        (HiValueNumber from', HiValueNumber to', _) -> pure (HiValueString (slice (toInt from') (toInt to') hiString))
        (HiValueNumber from', HiValueNull, _)       -> pure (HiValueString (slice (toInt from') (T.length hiString) hiString))
        (HiValueNull, HiValueNumber to', _)         -> pure (HiValueString (slice 0 (toInt to') hiString))
        _                                           -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorArityMismatch

evalStringOrListUnaryFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalStringOrListUnaryFunction function arguments
  | length arguments /= 1 = throwError HiErrorArityMismatch
  | otherwise = do
    a <- ExceptT (eval (head arguments))
    case (a) of
      (HiValueString a') ->
        case function of
          HiFunLength  -> pure (HiValueNumber (toRational (T.length a')))
          HiFunToUpper -> pure (HiValueString (T.toUpper a'))
          HiFunToLower -> pure (HiValueString (T.toLower a'))
          HiFunReverse -> pure (HiValueString (T.reverse a'))
          HiFunTrim    -> pure (HiValueString (T.strip a'))
          _            -> throwError HiErrorInvalidFunction
      (HiValueList l') ->
         case function of
          HiFunLength    -> pure (HiValueNumber (toRational (length l')))
          HiFunReverse   -> pure (HiValueList (fromList (reverse (toList l'))))
          _            -> throwError HiErrorInvalidFunction
      _ -> throwError HiErrorInvalidArgument

evalBinaryFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalBinaryFunction function arguments
  | length arguments /= 2 = throwError HiErrorArityMismatch
  | otherwise = do
    a <- ExceptT (eval (head arguments))
    b <- ExceptT (eval (arguments !! 1))
    case (a, b) of
      (HiValueString a', HiValueString b') ->
          case function of
              HiFunEquals    -> pure (HiValueBool (a' == b'))
              HiFunNotEquals -> pure (HiValueBool (a' /= b'))
              HiFunAdd       -> pure (HiValueString (a' <> b'))
              HiFunDiv       -> pure (HiValueString (a' <> singleton '/' <> b'))
              _              -> throwError HiErrorInvalidFunction
      (HiValueString a', HiValueNumber b') ->
          case function of
              HiFunMul -> pure (HiValueString (stimes (numerator b') a'))
              _        -> throwError HiErrorInvalidFunction
      (HiValueNumber b', HiValueString a') ->
          case function of
              HiFunMul -> pure (HiValueString (stimes (numerator b') a'))
              _        -> throwError HiErrorInvalidFunction
      (HiValueNumber a', HiValueNumber b') ->
          case function of
              HiFunAdd            -> pure (HiValueNumber (a' + b'))
              HiFunSub            -> pure (HiValueNumber (a' - b'))
              HiFunMul            -> pure (HiValueNumber (a' * b'))
              HiFunLessThan       -> pure (HiValueBool (a' < b'))
              HiFunNotLessThan    -> pure (HiValueBool (a' >= b'))
              HiFunGreaterThan    -> pure (HiValueBool (a' > b'))
              HiFunNotGreaterThan -> pure (HiValueBool (a' <= b'))
              HiFunEquals         -> pure (HiValueBool (a' == b'))
              HiFunNotEquals      -> pure (HiValueBool (a' /= b'))
              HiFunDiv            -> if b' /= 0 then pure (HiValueNumber (a' / b'))
                                     else throwError HiErrorDivideByZero
              _ -> throwError HiErrorInvalidFunction
      (HiValueBool a', HiValueBool b') ->
          case function of
              HiFunLessThan       -> pure (HiValueBool (a' < b'))
              HiFunNotLessThan    -> pure (HiValueBool (a' >= b'))
              HiFunGreaterThan    -> pure (HiValueBool (a' > b'))
              HiFunNotGreaterThan -> pure (HiValueBool (a' <= b'))
              HiFunEquals         -> pure (HiValueBool (a' == b'))
              HiFunNotEquals      -> pure (HiValueBool (a' /= b'))
              _                   -> throwError HiErrorInvalidFunction
      (HiValueBool _, HiValueNumber _) ->
          case function of
              HiFunLessThan       -> pure (HiValueBool True)
              HiFunNotLessThan    -> pure (HiValueBool False)
              HiFunGreaterThan    -> pure (HiValueBool False)
              HiFunNotGreaterThan -> pure (HiValueBool True)
              HiFunEquals         -> pure (HiValueBool False)
              HiFunNotEquals      -> pure (HiValueBool True)
              _                   -> throwError HiErrorInvalidFunction
      (HiValueNumber _, HiValueBool _) ->
          case function of
              HiFunLessThan       -> pure (HiValueBool False)
              HiFunNotLessThan    -> pure (HiValueBool True)
              HiFunGreaterThan    -> pure (HiValueBool True)
              HiFunNotGreaterThan -> pure (HiValueBool False)
              HiFunEquals         -> pure (HiValueBool False)
              HiFunNotEquals      -> pure (HiValueBool True)
              _                   -> throwError HiErrorInvalidFunction
      (HiValueFunction a', HiValueFunction b') ->
          case function of
              HiFunEquals    -> pure (HiValueBool (a' == b'))
              HiFunNotEquals -> pure (HiValueBool (a' /= b'))
              _              -> throwError HiErrorInvalidFunction
      (HiValueList a', HiValueList b') ->
          case function of
              HiFunAdd        -> pure (HiValueList (a' >< b'))
              _               -> throwError HiErrorInvalidFunction
      (HiValueList a', HiValueNumber b') ->
          case function of
              HiFunMul        -> pure (HiValueList (stimes (toInt b') a'))
              _               -> throwError HiErrorInvalidFunction
      (HiValueNumber a', HiValueList b') ->
          case function of
              HiFunMul        -> pure (HiValueList (stimes (toInt a') b'))
              _               -> throwError HiErrorInvalidFunction
      _ -> throwError HiErrorInvalidArgument

evalLogicalFunction :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalLogicalFunction function arguments =
  case function of
    HiFunIf ->  if length arguments /= 3 then throwError HiErrorArityMismatch
                else do
                  booleanCondition <- ExceptT (eval (head arguments))
                  evaluatedLhsExpr <- ExceptT (eval (arguments !! 1))
                  evaluatedRhsExpr <- ExceptT (eval (arguments !! 2))
                  case booleanCondition of
                    HiValueBool True  -> pure evaluatedLhsExpr
                    HiValueBool False -> pure evaluatedRhsExpr
                    _                 -> throwError HiErrorInvalidArgument
    HiFunAnd -> if length arguments /= 2 then throwError HiErrorArityMismatch
                else do
                  evaluatedLhsExpr <- ExceptT (eval (head arguments))
                  evaluatedRhsExpr <- ExceptT (eval (arguments !! 1))
                  case (evaluatedLhsExpr, evaluatedRhsExpr) of
                    (HiValueBool True, HiValueBool True) -> pure (HiValueBool True)
                    (HiValueBool _, HiValueBool _)       -> pure (HiValueBool False)
                    _                                    -> throwError HiErrorInvalidArgument
    HiFunOr ->  if length arguments /= 2 then throwError HiErrorArityMismatch
                else do
                  evaluatedLhsExpr <- ExceptT (eval (head arguments))
                  evaluatedRhsExpr <- ExceptT (eval (arguments !! 1))
                  case (evaluatedLhsExpr, evaluatedRhsExpr) of
                    (HiValueBool True, HiValueBool _) -> pure (HiValueBool True)
                    (HiValueBool _, HiValueBool True) -> pure (HiValueBool True)
                    (HiValueBool _, HiValueBool _)    -> pure (HiValueBool False)
                    _                                 -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidFunction


