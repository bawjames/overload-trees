module Types (show, arrow, fromList, (<>), apply, Type (..), Primitive (..)) where

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Tuple (snd)
import ParseTree
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as Pr

data Primitive
  = Int
  | Double
  | Float
  | Bool
  deriving (Eq, Ord, Show)

-- This is just a trie which allows a trie as keys
-- A Type of nested map singletons is equivalent to a functional type signature in functional languages
-- An empty Type is not valid
-- A variable (non-function) has a singleton Type, which cannot be an implicit type union.
-- A non-singleton, non-empty Type expresses a superposition of types, and the type can eventually be found by repeated application
data Type
  = Function (Map.Map Type Type)
  | Var Int
  | Concrete Primitive
  deriving (Eq, Ord)

type Environment = Map.Map Ident Scheme

type Subst = Map.Map Int Type

data Scheme = ForAll (Set.Set Int) Type

compose :: Subst -> Subst -> Subst
compose sA sB = Map.map (subst sA) sB <> sA

type VarSupply = ExceptT String (State Int)

class Types a where
  subst :: Subst -> a -> a
  freeVars :: a -> Set.Set Int

instance (Types a) => Types [a] where
  subst = map . subst
  freeVars = foldr (Set.union . freeVars) Set.empty

instance Types Environment where
  subst s = Map.map $ subst s
  freeVars = freeVars . Map.elems

instance Types Scheme where
  subst s (ForAll vars t) =
    ForAll vars $
      subst (foldr Map.delete s vars) t
  freeVars (ForAll vars t) = freeVars t \\ vars

instance Types Type where
  subst s v@(Var n) = fromMaybe v $ Map.lookup n s
  subst s (Function m) = Function $ Map.map (subst s) m
  subst s t = t

  freeVars (Function m) = Map.foldr ((<>) . freeVars) Set.empty m
  freeVars (Var n) = Set.singleton n
  freeVars _ = Set.empty

-- Type is not a Monoid because there is no valid empty Type, therefore no mempty.
-- This instance will throw an exception if either of the two arguments are not TyFun; use sconcat to handle all cases.
instance Semigroup Type where
  Function a <> Function b = Function $ Map.unionWith (<>) a b

instance Semigroup Scheme where
  ForAll vA (Function a) <> ForAll vB (Function b) =
    ForAll (vA <> vB) $ Function (a <> b)

instance Show Type where
  show = Pr.render . toDoc
    where
      toDoc (Var n) = Pr.text $ "t" ++ show n
      toDoc (Concrete p) = Pr.text $ show p
      toDoc (Function m) = Pr.vcat $
        flip map (Map.toList m) $
          \(k, v) -> parenLeft k <+> Pr.text "->" <+> toDoc v
      parenLeft ty@(Function _) = Pr.parens $ toDoc ty
      parenLeft ty = toDoc ty

-- Takes two types a and b and forms a function a -> b.
arrow :: Type -> Type -> Type
arrow = fmap Function . Map.singleton

infixr 5 `arrow`

-- Throws an exception if empty list.
fromList :: [Type] -> Type
fromList = foldr1 arrow

-- Applies an single argument to a function.
-- Throws an exception if the first argument is not a function.
-- Returns Nothing if a is not found in m.
apply :: Type -> Type -> Maybe Type
apply (Function m) a = Map.lookup a m

freshVar :: VarSupply Type
freshVar = gets Var <* modify (+ 1)

generalise :: Environment -> Type -> Scheme
generalise env t = ForAll vars t
  where
    vars = freeVars t \\ freeVars env

instantiate :: Scheme -> VarSupply Type
instantiate (ForAll vars t) = do
  let vars' = Set.toList vars
  let zipVars = mapM $ \var -> (var,) <$> freshVar
  s <- Map.fromList <$> zipVars vars'
  return $ subst s t

varBind :: Int -> Type -> VarSupply Subst
varBind u t
  | t == Var u = return Map.empty
  | u `Set.member` freeVars t =
      throwError $
        "Infinite type recursion, " ++ show u ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton u t

unify :: Type -> Type -> VarSupply Subst
unify (Var u) t = varBind u t
unify t (Var u) = varBind u t
unify (Concrete a) (Concrete b) | a == b = return Map.empty
unify tA tB =
  throwError $
    "Types do not unify, " ++ show tA ++ " vs. " ++ show tB

infer :: Environment -> Expr -> VarSupply (Subst, Type)
infer env (Identifier ident) =
  case Map.lookup ident env of
    Just sig -> (Map.empty,) <$> instantiate sig
    Nothing ->
      throwError $
        "Couldn't find " ++ ident ++ ", undefined or out of scope"
infer env (Literal lit) = return . (Map.empty,) <$> Concrete $
  case lit of
    IntegerLit _ -> Int
    BoolLit _ -> Bool
infer env (Lambda bound body) = do
  newVar <- freshVar
  let env' = Map.insert bound (ForAll Set.empty newVar) env
  (s, t) <- infer env' body
  return (s, subst s (arrow (subst s newVar) t))
infer env (Application eA eB) = do
  newVar <- freshVar
  (sA, tA) <- infer env eA
  (sB, tB) <- infer (subst sA env) eB
  tA' <- case apply tA tB of
    Just tA' -> return tA'
    Nothing -> throwError $ "No function defined for type " ++ show tB
  return (sB `compose` sA, tA')

runVarSupply :: VarSupply a -> Either String a
runVarSupply t = (`evalState` 0) $ runExceptT t
