{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amazonka.DynamoDB.Schema.Throughput
    ( ThroughputKind     (..)
    , ReadCapacityKind   (..)
    , WriteCapacityKind  (..)

    , ReadCapacity
    , WriteCapacity
    , Throughput

    , DynamoThroughput (..)
    ) where

import Data.Proxy (Proxy (..))

import GHC.TypeLits

import Network.AWS.DynamoDB

data ReadCapacityKind = ReadCapacity Nat

-- | One read capacity unit represents one strongly consistent read per second, or
-- two eventually consistent reads per second, for items up to 4 KB in size.
--
-- If you need to read an item that is larger than 4 KB, DynamoDB will need to
-- consume additional read capacity units. The total number of read capacity units
-- required depends on the item size, and whether you want an eventually
-- consistent or strongly consistent read.
--
-- The type parameter is of kind 'Nat'.
type ReadCapacity = 'ReadCapacity

data WriteCapacityKind = WriteCapacity Nat

-- | One write capacity unit represents one write per second for items up
-- to 1 KB in size.
--
-- If you need to write an item that is larger than 1 KB, DynamoDB will need
-- to consume additional write capacity units. The total number of write capacity
-- units required depends on the item size.
--
-- The type parameter is of kind 'Nat'.
type WriteCapacity = 'WriteCapacity

-- | A promoted kind and types for specifying provisioned throughput capacity.
data ThroughputKind = Throughput ReadCapacityKind WriteCapacityKind

-- | The provisioned throughput capacity you want to reserve for reads and writes.
--
-- DynamoDB will reserve the necessary resources to meet your throughput needs
-- while ensuring consistent, low-latency performance. You can also change your
-- provisioned throughput settings, increasing or decreasing capacity as needed.
--
-- The types parameters are of kind 'ReadCapacity' and 'WriteCapacity', respectively.
--
-- Regarding 'PartitionKey's,
--
-- @
-- Total Provisioned Throughput / Partitions = Throughput Per Partition
-- @
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ProvisionedThroughput.html How it works - Provisioned Throughput>.
type Throughput = 'Throughput

class DynamoThroughput a where
    -- | Get the DynamoDB 'ProvisionedThroughput' configuration.
    getThroughput :: Proxy a -> ProvisionedThroughput

instance ( KnownNat r
         , KnownNat w
         ) => DynamoThroughput (Throughput (ReadCapacity r) (WriteCapacity w)) where
    getThroughput _ =
        provisionedThroughput
            (fromInteger $ natVal (Proxy :: Proxy r))
            (fromInteger $ natVal (Proxy :: Proxy w))
