module Jarvis.Hint.IfStmtsMustUseBraces
  ( ifStmtsMustUseBracesHint
  ) where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax
import Jarvis.Hint.Type
import Jarvis.Idea (suggest, Idea)

fix (IfThen exp stmt) =
  (IfThen exp $ StmtBlock $ Block [BlockStmt stmt])

idea :: Stmt -> Idea
idea e = suggest "Do not use a single statement without braces in the if statement" e (fix e)
 
ifStmtsMustUseBracesHint :: TypeDeclHint
ifStmtsMustUseBracesHint typeDecl = 
  [idea e| e@(IfThen exp stmt) <- universeBi typeDecl, isSingleStatement stmt]
  where isSingleStatement stmt = case stmt of StmtBlock _ -> False
                                              _           -> True

