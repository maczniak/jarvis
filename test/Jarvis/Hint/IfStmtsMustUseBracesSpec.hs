{-# LANGUAGE QuasiQuotes #-}
module Jarvis.Hint.IfStmtsMustUseBracesSpec where

import Test.Hspec
import Text.RawString.QQ

import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))

import Jarvis.Hint.IfStmtsMustUseBraces
import Jarvis.Idea
import Jarvis.Hint.Helper

srcBad :: String
srcBad = [r|
class Foo {
  public void foo() {
    int i = 0;
    while (true) {
      if (i++ == 10) break;
    }
  }
}
|]

srcGood :: String
srcGood = [r|
class Bar {
  public void bar() {
    int i = 0;
    while (true) {
      if (i++ == 10) {
        break;
      }
    }
  }
}
|]

isIfStmtsMustUseBracesHint :: Idea -> Bool
isIfStmtsMustUseBracesHint Idea{ideaHint=ideaHint} =
  ideaHint == "Do not use a single statement without braces in the if statement"

spec :: Spec
spec = do
  describe "ifStmtsMustUseBracesHint" $ do
    it "warns a single statement without braces in the if statement" $
      runHint ifStmtsMustUseBracesHint srcBad `shouldSatisfy` (all isIfStmtsMustUseBracesHint) .&&. (not . null)
    it "detects nothing with a single statement with braces" $
      runHint ifStmtsMustUseBracesHint srcGood `shouldBe` []

