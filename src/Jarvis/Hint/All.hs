module Jarvis.Hint.All
  ( builtin
  , builtinHints
  , HintBuiltin(..)
  ) where

import Jarvis.Hint.Type
import Jarvis.Hint.BadCovariantDefinitionOfEquals
import Jarvis.Hint.BadComparisonWithBoolean
import Jarvis.Hint.UnconditionalIfStatement
import Jarvis.Hint.IfStmtsMustUseBraces

data HintBuiltin =
    HintBadCovariantDefinitionOfEquals
  | HintBadComparisonWithBoolean
  | HintUnconditionalIfStatement
  | HintIfStmtsMustUseBraces
  deriving (Show,Eq,Ord,Bounded,Enum)

builtin :: HintBuiltin -> Hint
builtin x = case x of
  HintBadCovariantDefinitionOfEquals -> typeDecl badCovariantDefinitionOfEqualsHint
  HintBadComparisonWithBoolean -> typeDecl badComparisonWithBooleanHint
  HintUnconditionalIfStatement -> typeDecl unconditionalIfStatementHint
  HintIfStmtsMustUseBraces -> typeDecl ifStmtsMustUseBracesHint
  where
    typeDecl x = mempty{hintTypeDecl=x}

builtinHints :: [(String, Hint)]
builtinHints = [(show h, builtin h) | h <- [minBound .. maxBound]]

