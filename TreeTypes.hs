module Types (show, arrow, fromList, (<>), apply, Type (..), Primitive (..)) where

import Data.List (intercalate)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple (snd)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as Pr

data Primitive
  = TyInt
  | TyDouble
  | TyFloat
  | TyBool
  deriving (Eq, Ord, Show)

-- This is just a trie which allows a trie as keys
-- A Type of nested map singletons is equivalent to a functional type signature in functional languages
-- An empty Type is not valid
-- A variable (non-function) has a singleton Type, which cannot be an implicit type union.
-- A non-singleton, non-empty Type expresses a superposition of types, and the type can eventually be found by repeated application
data Type
  = TyFun (Map.Map Type Type)
  | TyVar Int
  | TyCon Primitive
  deriving (Eq, Ord)

-- Type is not a Monoid because there is no valid empty Type, therefore no mempty.
-- This instance will throw an exception if either of the two arguments are not TyFun; use sconcat to handle all cases.
instance Semigroup Type where
  TyFun a <> TyFun b = TyFun $ Map.unionWith (<>) a b

instance Show Type where
  show = Pr.render . toDoc
    where
      toDoc (TyVar n) = Pr.text $ "t" ++ show n
      toDoc (TyCon p) = Pr.text $ show p
      toDoc (TyFun m) = Pr.vcat $
        flip map (Map.toList m) $
          \(k, v) -> parenLeft k <+> Pr.text "->" <+> toDoc v
      parenLeft ty@(TyFun _) = Pr.parens $ toDoc ty
      parenLeft ty = toDoc ty

-- Takes two types a and b and forms a function a -> b.
arrow :: Type -> Type -> Type
arrow = fmap TyFun . Map.singleton

infixr 5 `arrow`

-- Throws an exception if empty list.
fromList :: [Type] -> Type
fromList = foldr1 arrow

-- Applies an single argument to a function.
-- Throws an exception if the first argument is not a function.
-- Returns Nothing if a is not found in m.
apply :: Type -> Type -> Maybe Type
apply (TyFun m) a = Map.lookup a m

-- The following is for testing purposes and should eventually be removed.
bool = TyCon TyBool

double = TyCon TyDouble

float = TyCon TyFloat

int = TyCon TyInt

a = fromList [int, int, int]

b = fromList [int, float, float]

c = fromList [a <> b, int, float]
