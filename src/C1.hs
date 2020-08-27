module C1
    ( c1Entry
    ) where

-- Ex 1.3
data Nat = Zero | Succ Nat deriving Show
data NatPlus = One | SuccP NatPlus deriving Show

foldn :: (a, a -> a) -> Nat -> a
foldn (b, _) Zero = b
foldn (b, f) (Succ n) = f (foldn (b, f) n)

foldnp :: (a, a -> a) -> NatPlus -> a
foldnp (b, _) One = b
foldnp (b, f) (SuccP n) = f (foldnp (b, f) n)

nat2Plus :: Nat -> NatPlus
nat2Plus = foldn (One, SuccP)

plus2Nat :: NatPlus -> Nat
plus2Nat = foldnp (Zero, Succ)

idNat :: Nat -> Nat
idNat = plus2Nat . nat2Plus

idNatP :: NatPlus -> NatPlus
idNatP = nat2Plus . plus2Nat

c1Entry :: IO ()
c1Entry = do
  print $ show $ idNat (Succ (Succ (Succ Zero)))
  print $ show $ idNatP (SuccP (SuccP One))
  print $ show $ nat2Plus (Succ (Succ (Succ Zero)))