{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Network.AWS.DynamoDB.Expression.Placeholder
    ( Substitute (..)
    , NamesAndValues
    , Values
    ) where

import Control.Lens                     (Lens', modifying, uses, _1, _2)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Maybe        (MaybeT)
import Control.Monad.Trans.State.Strict (StateT)

import Data.Hashable          (Hashable (..))
import Data.HashMap.Strict    (HashMap)
import Data.Semigroup         ((<>))
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector.Unboxed    (Vector)

import Network.AWS.DynamoDB.Value (DynamoValue (..), Value)

import qualified Data.HashMap.Strict    as Map
import qualified Data.Text.Lazy.Builder as Build
import qualified Data.Vector.Unboxed    as Vector

type NamesAndValues = (HashMap Text Builder, HashMap Value Builder)
type Values         = HashMap Value Builder

class Monad m => Substitute m where
    substituteName  :: Text -> m Builder
    substituteValue :: DynamoValue a => a -> m Builder

instance Substitute m => Substitute (MaybeT m) where
    substituteName  = lift . substituteName
    substituteValue = lift . substituteValue

instance Monad m => Substitute (StateT NamesAndValues m) where
    substituteName  = substitute '#' _1
    substituteValue = substitute ':' _2 . toValue

instance Monad m => Substitute (StateT Values m) where
    substituteName  = pure . Build.fromText
    substituteValue = substitute ':' id . toValue

substitute :: (Monad m, Hashable a, Eq a)
           => Char
           -> Lens' s (HashMap a Builder)
           -> a
           -> StateT s m Builder
substitute prefix l k = do
    mn <- uses l (Map.lookup k)
    case mn of
        Just  n -> return n
        Nothing -> do
            let n = Build.singleton prefix <> encode (hash k)
            modifying l (Map.insert k n)
            return n

-- | Encode an 'Int' to Base62.
encode :: Int -> Builder
encode = Build.fromString . map ((Vector.!) alphabet) . toDigits

-- | Base62 alphabet.
alphabet :: Vector Char
alphabet = Vector.fromList $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

-- | Get each individual digit of an 'Integral' value.
toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits x
    | x < 0     = 0 : digits
    | otherwise = digits
  where
    (rest, i) = quotRem (abs x) 62

    digits = i : toDigits rest
