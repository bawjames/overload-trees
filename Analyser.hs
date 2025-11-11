module Analyser where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import ParseTree
import Parser (entry)
import Text.Parsec.String (parseFromFile)

data Scheme = Forall [Ident] Type

instance Show Scheme where
  show (Forall is t) =
    "∀(" ++ intercalate ", " is ++ ")"
    ++ " " ++ show t

data Type
  = Function Type Type
  | Concrete Primitive
  | Var Int
  deriving (Eq)

instance Show Type where
  show (Function a b) = show a ++ " -> " ++ show b
  show (Concrete prim) = show prim
  show (Var int) = "t" ++ show int

data Primitive
  = Integer
  | Bool
  deriving (Show, Eq)

type Environment = Map.Map Ident Type

data Constraint = Constraint Type Type
  deriving (Show)

type Infer = ReaderT Environment (WriterT [Constraint] IdGen)

type IdGen = State Int

freshVar :: Infer Type
freshVar = do
  n <- get
  put (n + 1)
  return $ Var n

infer :: Expr -> Infer (Maybe Type)
infer (Literal lit) = return . Just . Concrete $
  case lit of
    IntegerLit _ -> Integer
    BoolLit _ -> Bool
infer (Identifier ident) = asks $ Map.lookup ident
infer (Lambda bound expr) = do
  var <- freshVar
  env <- ask
  let env' = Map.insert bound var env
  body <- local (const env') $ infer expr
  return $ Function var <$> body
infer (Application a b) = do -- Is this correct?
    mta <- infer a
    mtb <- infer b
    case (mta, mtb) of
        (Just ta, Just tb) -> do
            result <- freshVar
            tell [Constraint tb (Function ta result)]
            return $ Just result
        _ -> return Nothing

runInfer :: Environment -> Expr -> (Maybe Type, [Constraint])
runInfer env expr =
  evalState y 0
    where
      x = runReaderT (infer expr) env
      y = runWriterT x

go :: IO ()
go = do
  let expr = Lambda "x" (Lambda "y" (Literal (IntegerLit 1)))
  print $ runInfer Map.empty expr
