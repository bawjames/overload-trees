module ParseTree where

data Assign = Assign Ident Expr
  deriving (Show)

data Expr
  = Application Expr Expr
  | Lambda Ident Expr
  | Identifier Ident
  | Literal Lit
  deriving (Show)

data Lit
  = IntegerLit Integer
  | BoolLit Bool
  deriving (Show)

type Ident = String
