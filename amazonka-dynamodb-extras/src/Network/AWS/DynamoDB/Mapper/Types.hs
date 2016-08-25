{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Mapper.Types where

import Data.Proxy         (Proxy (..))
import Data.Text          (Text)
import Data.Type.Bool     (If, type (||))
import Data.Type.Equality (type (==))

import GHC.TypeLits

import Network.AWS.DynamoDB

import qualified Data.Text as Text

data Table (name :: Symbol) schema options

data Key (name :: Symbol) (key :: KeyType)

data Attribute (name :: Symbol) value

data ReadCapacity  (read  :: Nat)
data WriteCapacity (write :: Nat)

data Throughput read write

data Stream (view :: StreamViewType)

data GlobalIndex (name :: Symbol) keys options
data LocalIndex  (name :: Symbol)      options

data Project (proj :: ProjectionType)

infixr 8 :#, :*:

-- | A column specifier.
data a :# b

-- | A deserialized product.
data a :*: b = a :*: b
    deriving (Show)

type family (∈) x xs where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs

type family (++) xs ys where
    (++) xs       '[]       = xs
    (++) '[]       ys        = ys
    (++) (x ': xs) ys = x ': (xs ++ ys)

type family Remove x xs where
    Remove x '[]       = '[]
    Remove x (x ': ys) = ys
    Remove x (y ': ys) = y ': Remove x ys

type family NonIntersecting xs ys where
    NonIntersecting '[]       ys        = '[]
    NonIntersecting xs       '[]        = xs
    NonIntersecting (x ': xs) (x ': ys) = NonIntersecting xs ys
    NonIntersecting (x ': xs) ys        =
          If (x ∈ ys)
             (NonIntersecting xs (Remove x ys))
             '[x]

-- type family FilterKeys a where
--     FilterKeys (Table       n r w    :# b)             = FilterKeys b
--     FilterKeys (GlobalIndex n r w ks :# b)             = FilterKeys b
--     FilterKeys (LocalIndex  n ks     :# b)             = FilterKeys b
--     FilterKeys (HashKey     n v      :# Attribute n v) = HashKey  n v
--     FilterKeys (HashKey     n v      :# Field     n v) = HashKey  n v
--     FilterKeys (HashKey     n v      :# b)             = HashKey  n v :# FilterKeys b
--     FilterKeys (RangeKey    n v      :# Attribute n v) = RangeKey n v
--     FilterKeys (RangeKey    n v      :# Field     n v) = RangeKey n v
--     FilterKeys (RangeKey    n v      :# b)             = RangeKey n v :# FilterKeys b

-- getThroughput :: (KnownNat r, KnownNat w)
--               => Proxy r
--               -> Proxy w
--               -> ProvisionedThroughput
-- getThroughput r w =
--     provisionedThroughput (fromIntegral (natVal r))
--                           (fromIntegral (natVal w))

symbolText :: KnownSymbol n => proxy n -> Text
symbolText = Text.pack . symbolVal
