{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.AWS.DynamoDB.Schema.Throughput where

import Control.Applicative ((<|>))

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Types

class DynamoThroughput a where
    maybeThroughput :: Proxy a -> Maybe ProvisionedThroughput
    maybeThroughput _ = Nothing

defaultThroughput :: ProvisionedThroughput
defaultThroughput = provisionedThroughput 1 1

getThroughput :: DynamoThroughput a => Proxy a -> ProvisionedThroughput
getThroughput = fromMaybe defaultThroughput . maybeThroughput

instance DynamoThroughput o => DynamoThroughput (Table n s o) where
    maybeThroughput _ = maybeThroughput (Proxy :: Proxy o)

instance ( DynamoThroughput a
         , DynamoThroughput b
         ) => DynamoThroughput (a :# b) where
    maybeThroughput _ =
            maybeThroughput (Proxy :: Proxy a)
        <|> maybeThroughput (Proxy :: Proxy b)

instance ( KnownNat r
         , KnownNat w
         ) => DynamoThroughput (Throughput (ReadCapacity r) (WriteCapacity w)) where
    maybeThroughput _ = pure $
        provisionedThroughput
            (fromIntegral (natVal (Proxy :: Proxy r)))
            (fromIntegral (natVal (Proxy :: Proxy w)))

instance DynamoThroughput (PartitionKey      n h)
instance DynamoThroughput (SortKey           n r)
instance DynamoThroughput (Attribute         n v)
instance DynamoThroughput (IndexPartitionKey n)
instance DynamoThroughput (IndexSortKey      n)
instance DynamoThroughput (Throughput        r w)
instance DynamoThroughput (Stream            v)
instance DynamoThroughput (Project           p)
