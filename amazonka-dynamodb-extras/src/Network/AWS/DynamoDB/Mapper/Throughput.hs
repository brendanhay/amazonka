{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AWS.DynamoDB.Mapper.Throughput where

import Control.Applicative ((<|>))

import Data.Proxy (Proxy (..))

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper.Types

class DynamoThroughput a where
    getThroughput :: Proxy a -> Maybe ProvisionedThroughput
    getThroughput _ = Nothing

instance DynamoThroughput o => DynamoThroughput (Table n s o) where
    getThroughput _ = getThroughput (Proxy :: Proxy o)

instance ( DynamoThroughput a
         , DynamoThroughput b
         ) => DynamoThroughput (a :# b) where
    getThroughput _ =
            getThroughput (Proxy :: Proxy a)
        <|> getThroughput (Proxy :: Proxy b)

instance ( KnownNat r
         , KnownNat w
         ) => DynamoThroughput (Throughput (ReadCapacity r) (WriteCapacity w)) where
    getThroughput _ = pure $
        provisionedThroughput
            (fromIntegral (natVal (Proxy :: Proxy r)))
            (fromIntegral (natVal (Proxy :: Proxy w)))

instance DynamoThroughput (Key        n k)
instance DynamoThroughput (Throughput r w)
instance DynamoThroughput (Stream     v)
instance DynamoThroughput (Project    p)
