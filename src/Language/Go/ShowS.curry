------------------------------------------------------------------------------
--- This library contains a simple pretty printer for showing
--- [Go](https://golang.org/) programs in `ShowS` format instead
--- of strings.
---
--- @author Jonas Boehm, Michael Hanus
--- @version July 2021
------------------------------------------------------------------------------

module Language.Go.ShowS
  ( showGoProg, showGoStat, showGoExpr )
 where

import Language.Go.Types

--- Shows a Go program as a string in Go syntax.
showGoProg :: GoProg -> ShowS
showGoProg (GoProg package imports decls) =
  showString "package " . showString package . showLn . showLn .
  showGoImports imports .
  mapS (\x -> showGoTopLevelDecl x . showLn) decls

--- Shows a list of imports as a string in Go syntax.
showGoImports :: [String] -> ShowS
showGoImports []       = showLn
showGoImports (x : xs) =
  showString "import " . shows x . showLn . showGoImports xs

--- Shows a Go top-level declaration as a string in Go syntax.
showGoTopLevelDecl :: GoTopLevelDecl -> ShowS
showGoTopLevelDecl (GoTopLevelDecl stat)     = showGoStat 0 stat
showGoTopLevelDecl (GoTopLevelFuncDecl func) = showGoFuncDecl func

--- Shows a Go function declaration as a string in Go syntax.
showGoFuncDecl :: GoFuncDecl -> ShowS
showGoFuncDecl (GoFuncDecl name params results body) =
  showString "func " . showString name . showString "( " .
  showGoCommaList showGoParam params . showString " )( " .
  showGoCommaList showGoParam results . showString " ){\n" .
  mapS (showGoStat 1) body . showString "}\n"

--- Shows a Go statement as a string in Go syntax with indenting.
--- @param n      - number of spaces to indent
--- @param gostat - the Go statement to show
showGoStat :: Int -> GoStat -> ShowS
showGoStat n (GoConstDecl ids t [])          =
  indent n . showString "const " . showGoCommaList showString ids .
  showChar ' ' . showString t . showLn
showGoStat n (GoConstDecl ids t exprs@(_:_)) =
  indent n . showString "const " .
  showGoCommaList showString ids . showChar ' ' . showString t .
  showString " = " . showGoCommaList showGoExpr exprs . showLn
showGoStat n (GoVarDecl ids t [])            =
  indent n . showString "var " . showGoCommaList showString ids . showChar ' ' .
  showString t . showLn
showGoStat n (GoVarDecl ids t exprs@(_:_))   =
  indent n . showString "var " .
  showGoCommaList showString ids . showChar ' ' . showString t .
  showString " = " . showGoCommaList showGoExpr exprs . showLn
showGoStat n (GoShortVarDecl ids exprs)      =
  indent n . showGoCommaList showString ids . showString " := " .
  showGoCommaList showGoExpr exprs . showLn
showGoStat n (GoExprStat expr)               =
  indent n . showGoExpr expr . showLn
showGoStat n (GoAssign exprs1 op exprs2)     =
  indent n .
  showGoCommaList showGoExpr exprs1 . showChar ' ' . showString op .
  showChar ' ' . showGoCommaList showGoExpr exprs2 . showLn
showGoStat _ (GoEmpty)                       = showLn
showGoStat n (GoReturn [])                   = indent n . showString "return\n"
showGoStat n (GoReturn exprs@(_:_))          =
  indent n . showString "return( " .
  showGoCommaList showGoExpr exprs . showString " )\n"
showGoStat n (GoBreak)                       = indent n . showString "break\n"
showGoStat n (GoContinue)                    =
  indent n . showString "continue\n"
showGoStat n (GoBlock stats)                 =
  indent n . showString "{\n" .
  mapS (showGoStat (n+1)) stats . indent n . showString "}\n"
showGoStat n (GoIf expr block1 block2)       =
  indent n . showString "if( " .
  showGoExpr expr . showString " ){\n" . mapS (showGoStat (n+1)) block1 .
  indent n . showString "}else {\n" . mapS (showGoStat (n+1)) block2 .
  indent n . showString "}\n"
showGoStat n (GoExprSwitch expr branches)    =
  indent n . showString "switch " .
  showGoExpr expr . showString "{\n" .
  mapS (goShowExprBranch (n+1)) branches . indent n . showString "}\n"

--- Shows a Go expression as a string in Go syntax.
showGoExpr :: GoExpr -> ShowS
showGoExpr (GoBoolLit b)                =
  showString $ if b then "true" else "false"
showGoExpr (GoIntLit i)                 = shows i
showGoExpr (GoFloatLit f)               = shows f
showGoExpr (GoStringLit s)              = shows s
showGoExpr (GoByteLit c)                = shows c
showGoExpr (GoCompositeLit t exprs)     =
  showString t . showString "{ " .
  showGoCommaList showGoExpr exprs . showString " }"
showGoExpr (GoOpName s)                 = showString s
showGoExpr (GoOpExpr expr)              = showParen True (showGoExpr expr)
showGoExpr (GoConversion t expr)        =
  showString t . showString "( "  . showGoExpr expr . showString " )"
showGoExpr (GoSelector expr s)          =
  showGoExpr expr . showChar '.' . showString s
showGoExpr (GoIndex expr1 expr2)        =
  showGoExpr expr1 . showString "[ " . showGoExpr expr2 . showString " ]"
showGoExpr (GoSlice expr1 expr2 expr3)  =
  showGoExpr expr1 . showString "[ " . showGoExpr expr2 . showString " : " .
  showGoExpr expr3 . showString " ]"
showGoExpr (GoVariadic expr)            = showGoExpr expr . showString "..."
showGoExpr (GoCall expr exprs)          =
  showGoExpr expr . showString "( " . showGoCommaList showGoExpr exprs .
  showString " )"
showGoExpr (GoUnaryExpr s expr)         =
  showString s . showChar ' ' . showGoExpr expr
showGoExpr (GoBinaryExpr expr1 s expr2) =
  showGoExpr expr1 . showChar ' ' . showString s . showChar ' ' .
  showGoExpr expr2

--- Shows a Go expression branch as a String in Go Syntax.
--- @param n      - number of spaces to indent
--- @param branch - branch to show
goShowExprBranch :: Int -> GoExprBranch -> ShowS
goShowExprBranch n (GoExprDefault stats)      =
  indent n . showString "default:\n" .
  mapS (showGoStat (n+1)) stats
goShowExprBranch n (GoExprBranch exprs stats) =
  indent n . showString "case " .
  showGoCommaList showGoExpr exprs . showString ":\n" .
  mapS (showGoStat (n+1)) stats

--- Shows a Go parameter as a string in Go syntax.
showGoParam :: GoParam -> ShowS
showGoParam (GoParam []       t) = showString t
showGoParam (GoParam xs@(_:_) t) =
  showGoCommaList showString xs . showChar ' ' . showString t

--- Shows a List of a Go type as a comma separated list in go syntax.
--- @param f    - function to show the Go type as a string in go syntax
--- @param list - list of a Go type
showGoCommaList :: (a -> ShowS) -> [a] -> ShowS
showGoCommaList _ []           = id
showGoCommaList f [x]          = f x
showGoCommaList f (x:xs@(_:_)) = f x . showString ", " . showGoCommaList f xs

--- Creates a string of blanks with length 4*n
indent :: Int -> ShowS
indent n = showString (replicate (4*n) ' ')

mapS :: (a -> ShowS) -> [a] -> ShowS
mapS _ []     = id
mapS f (x:xs) = f x . mapS f xs

showLn :: ShowS
showLn = showChar '\n'

------------------------------------------------------------------------------
