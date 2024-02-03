import Data.Void (Void)
import Hi.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import Hi.Evaluator (eval)
import Hi.Parser (parse)
import Hi.Pretty (prettyValue)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Text.Megaparsec.Error (ParseErrorBundle)

testPrettyValueFunction :: SpecWith ()
testPrettyValueFunction =
  describe "==== Pretty printing === " $ do
    describe "Booleans" $ do
      testPrettyValueHelper "Test 'true'" (HiValueBool True) "true"
      testPrettyValueHelper "Test 'false'" (HiValueBool False) "false"

    describe "Booleans" $ do
      testPrettyValueHelper "Test 'add'" (HiValueFunction HiFunAdd) "add"
      testPrettyValueHelper "Test 'sub'" (HiValueFunction HiFunSub) "sub"
      testPrettyValueHelper "Test 'div'" (HiValueFunction HiFunDiv) "div"
      testPrettyValueHelper "Test 'mul'" (HiValueFunction HiFunMul) "mul"

    describe "Integers" $ do
      testPrettyValueHelper "Test 'integers'" (HiValueNumber 8) "8"
      testPrettyValueHelper "Test 'integers'" (HiValueNumber 42) "42"
      testPrettyValueHelper "Test 'integers'" (HiValueNumber 239) "239"
      testPrettyValueHelper "Test 'integers'" (HiValueNumber (-718)) "-718"

    describe "Fractions" $ do
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (1 / 3)) "1/3"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (-1 / 3)) "-1/3"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (3 / 11)) "3/11"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (5 / 3)) "1 + 2/3"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (-71 / 7)) "-10 - 1/7"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber (5 / 2)) "2.5"
      testPrettyValueHelper "Test 'fraction'" (HiValueNumber 3.14) "3.14"

testParseFunction :: SpecWith ()
testParseFunction =
  describe "\n==== Parsing ====" $ do
    describe "Add" $ do
      testParseFunctionHelper "add(3, 5)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunAdd))
              [HiExprValue (HiValueNumber 3), HiExprValue (HiValueNumber 5)]
          )
      testParseFunctionHelper "add(add(1, 2), 3)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunAdd))
              [ HiExprApply
                  (HiExprValue (HiValueFunction HiFunAdd))
                  [HiExprValue (HiValueNumber 1), HiExprValue (HiValueNumber 2)],
                HiExprValue (HiValueNumber 3)
              ]
          )
    describe "Multiplication" $ do
      testParseFunctionHelper "mul(4, 6)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunMul))
              [HiExprValue (HiValueNumber 4), HiExprValue (HiValueNumber 6)]
          )
    describe "Subtraction" $ do
      testParseFunctionHelper "sub(10, 5)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunSub))
              [HiExprValue (HiValueNumber 10), HiExprValue (HiValueNumber 5)]
          )
    describe "Division" $ do
      testParseFunctionHelper "div(14, 2)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunDiv))
              [HiExprValue (HiValueNumber 14), HiExprValue (HiValueNumber 2)]
          )
      testParseFunctionHelper "div(1, 0)" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunDiv))
              [HiExprValue (HiValueNumber 1), HiExprValue (HiValueNumber 0)]
          )

    describe "Mixed" $ do
      testParseFunctionHelper "mul(sub(10, 5), div(14, 2))" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunMul))
              [ HiExprApply
                  (HiExprValue (HiValueFunction HiFunSub))
                  [HiExprValue (HiValueNumber 10), HiExprValue (HiValueNumber 5)],
                HiExprApply
                  (HiExprValue (HiValueFunction HiFunDiv))
                  [HiExprValue (HiValueNumber 14), HiExprValue (HiValueNumber 2)]
              ]
          )
      testParseFunctionHelper "add(sub(10, 3), div(14, 2))" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunAdd))
              [ HiExprApply
                  (HiExprValue (HiValueFunction HiFunSub))
                  [HiExprValue (HiValueNumber 10), HiExprValue (HiValueNumber 3)],
                HiExprApply
                  (HiExprValue (HiValueFunction HiFunDiv))
                  [HiExprValue (HiValueNumber 14), HiExprValue (HiValueNumber 2)]
              ]
          )

      testParseFunctionHelper "3 * 3 * 3" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunMul))
              [HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprValue (HiValueNumber (3)), HiExprValue (HiValueNumber (3))], HiExprValue (HiValueNumber (3))]
          )

      testParseFunctionHelper "2 + 2 * 3 == (2 + 2) * 3" $
        Right
          ( HiExprApply
              (HiExprValue (HiValueFunction HiFunEquals))
              [ HiExprApply
                  (HiExprValue (HiValueFunction HiFunAdd))
                  [ HiExprValue (HiValueNumber (2)),
                    HiExprApply
                      (HiExprValue (HiValueFunction HiFunMul))
                      [ HiExprValue (HiValueNumber (2)),
                        HiExprValue
                          ( HiValueNumber(3))
                      ]
                  ],
                HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (2)), HiExprValue (HiValueNumber (2))], HiExprValue (HiValueNumber (3))]
              ]
          )

testEvalFunction :: SpecWith ()
testEvalFunction =
  describe "\n==== Evaluating ====" $ do
    describe "Simple math" $ do
      testEvalFunctionHelper "2 + 2" "4"
      testEvalFunctionHelper "239 * 239 * (239 - 1) * (239 - (239 + 1) + 1)" "0"
      testEvalFunctionHelper "(2 + 2) * 3" "12"
      testEvalFunctionHelper "2 + 2 * 3" "8"
      testEvalFunctionHelper "0 * 2 + 3 == 2 + 3" "false"
      testEvalFunctionHelper "10 == (9 + (10 - 9) * 0 + 1)" "true"
      testEvalFunctionHelper "if (1==1, add, div) (3,4)" "7"
      testEvalFunctionHelper "if (1, add, div) (3,4)" "error"
      testEvalFunctionHelper "  2 <       3" "true"
      testEvalFunctionHelper "  2 <       3 < 4" "error"
      testEvalFunctionHelper "  add(1,2)" "3"
      testEvalFunctionHelper "  add(1,div)" "error"
      testEvalFunctionHelper "true" "true"
      testEvalFunctionHelper "false" "false"
      testEvalFunctionHelper "add" "add"
      testEvalFunctionHelper "add == add" "true"
      testEvalFunctionHelper "add == mul" "false"
      testEvalFunctionHelper "if(1<2<3<239, 10,203) add(1,2)" "error"
      testEvalFunctionHelper "if(true || false, 10, 20 * 3 * \t\t\t9\t\t\t)" "10"
      testEvalFunctionHelper "if(add(1,2) == div(6,2), 2 * 3 * 9, 0)" "54"
      testEvalFunctionHelper "true || (1 > 2)" "true"
      

main :: IO ()
main = hspec $ do
  testPrettyValueFunction
  testParseFunction
  testEvalFunction

{-
    Some helpers for testing...
-}
evalAndPrettifyToStrOrErr :: String -> String
evalAndPrettifyToStrOrErr input =
  case parse input of
    Right expr -> case eval expr of
      Right (Right value) -> show (prettyValue value)
      _ -> "error"
    Left _ -> "error"

testEvalFunctionHelper :: String -> String -> SpecWith ()
testEvalFunctionHelper input expected =
  it ("Evaluates '" ++ input ++ "' to '" ++ expected ++ "'") $
    evalAndPrettifyToStrOrErr input `shouldBe` expected

testParseFunctionHelper :: String -> Either (ParseErrorBundle String Void) HiExpr -> SpecWith ()
testParseFunctionHelper input expected =
  it ("Parses '" ++ input ++ "'") $
    parse input `shouldBe` expected

testPrettyValueHelper :: String -> HiValue -> String -> SpecWith ()
testPrettyValueHelper label value expected =
  it (label ++ " for " ++ show value) $
    show (prettyValue value) `shouldBe` expected
