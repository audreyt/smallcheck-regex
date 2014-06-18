{-# LANGUAGE KindSignatures, DataKinds, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Test.SmallCheck.Regex (Matching(..)) where
import Test.SmallCheck (smallCheck)
import Test.SmallCheck.Series (Serial(..), generate, getDepth, list, listM, Series)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Regex.Genex (genex, genexPure)

data Matching :: Symbol -> * where
    Matching :: KnownSymbol regex => Matching regex
    Match :: String -> Matching regex

instance Show (Matching regex) where
    show m@Matching = symbolVal m
    show (Match string) = string

-- TODO: Separate definition for MonadIO

instance (Monad m, KnownSymbol regex) => Serial m (Matching regex) where
    series = generate $ \d -> if False then [m] else Match `fmap` (take d $ genexPure [r])
        where
        r = show m
        m = Matching

{-
prop :: Matching "a+b+" -> Bool
prop s = length (show s) /= 7

main = do
    print =<< listM 10 (series :: Series IO (Matching "a?b*c+"))
    smallCheck 10000 prop
-}
