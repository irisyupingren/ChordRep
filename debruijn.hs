{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import Data.Foldable as Foldable
import Data.Traversable

newtype Scope f a = Scope (f a) deriving (Eq,Show,Read,Functor,Foldable,Traversable)
data Exp a
  = F a
  | B !Int
  | App (Exp a) (Exp a)
  | Lam (Scope Exp a)
  deriving (Eq,Show,Read,Functor,Foldable,Traversable)
-- show
abstract :: Eq a => a -> Exp a -> Scope Exp a
abstract me expr = Scope (letmeB 0 expr) where
  letmeB this (F you) | you == me = B this
                      | otherwise = F you
  letmeB this (B that)            = B that
  letmeB this (App fun arg)       = letmeB this fun `App` letmeB this arg
  letmeB this (Lam (Scope body))  = Lam $ Scope $ letmeB (succ this) body

instantiate :: Exp a -> Scope Exp a -> Exp a
instantiate what (Scope body) = what'sB 0 body where
  what'sB this (B that) | this == that = what
                        | otherwise    = B that
  what'sB this (F you)                 = F you
  what'sB this (App fun arg)           = what'sB this fun `App` what'sB this arg
  what'sB this (Lam (Scope body))      = Lam $ Scope $ what'sB (succ this) body
-- /show


-- show
closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)

isClosed :: Foldable f => f a -> Bool
isClosed = Foldable.all (const False)
-- /show

-- main = putStrLn "It typechecks, and is even slightly interesting."
