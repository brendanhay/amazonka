{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AWS.DynamoDB.Schema.Throughput
    ( ReadCapacity
    , WriteCapacity
    , Throughput

    , DynamoThroughput (..)
    ) where

import Data.Proxy (Proxy (..))

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Types

class DynamoThroughput a where
    -- | Get the DynamoDB 'ProvisionedThroughput' configuration.
    getThroughput :: Proxy a -> ProvisionedThroughput

instance DynamoThroughput t => DynamoThroughput (Table n a t s is) where
    getThroughput _ = getThroughput (Proxy :: Proxy t)

instance ( KnownNat r
         , KnownNat w
         ) => DynamoThroughput (Throughput (ReadCapacity r) (WriteCapacity w)) where
    getThroughput _ =
        provisionedThroughput
            (fromInteger $ natVal (Proxy :: Proxy r))
            (fromInteger $ natVal (Proxy :: Proxy w))
