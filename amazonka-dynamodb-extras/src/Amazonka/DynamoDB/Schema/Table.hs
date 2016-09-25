{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Amazonka.DynamoDB.Schema.Table
    ( Table
    , DynamoTable (..)

    -- * Creating Tables
    , CreateTable
    , getCreateTable

    -- * Updating Tables
    , UpdateTable
    -- , getUpdateTable

    -- * Deleting Tables
    , DeleteTable
--    , delete

    ) where

import Amazonka.DynamoDB.Item.Attribute
import Amazonka.DynamoDB.Schema.Attribute
import Amazonka.DynamoDB.Schema.Index
import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Serialize
import Amazonka.DynamoDB.Schema.Stream
import Amazonka.DynamoDB.Schema.Throughput

import Control.Lens ((.~), (?~))

import Data.Foldable         (toList)
import Data.Function         ((&))
import Data.Functor.Identity (Identity)
import Data.Proxy            (Proxy (..))
import Data.Text             (Text)

import GHC.Exts     (Constraint)
import GHC.TypeLits (KnownSymbol, Symbol)

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

-- | A DynamoDB table schema.
--
-- The kinds of the parameters are:
--
--    * The 'Symbol' table name.
--    * An attribute schema defined using the desired 'AttributeKind's. ':#' can be used for composition.
--    * The provisioned throughput specified by 'ThroughputKind'.
--    * The streaming specification settings specified by 'StreamingKind'.
--    * An optionally empty list of indexes defined using  'SecondaryIndexKind'.
--
data Table
    (name       :: Symbol)
    (attributes :: AttributeKind)
    (throughput :: ThroughputKind)
    (streaming  :: StreamingKind)
    (indexes    :: [SecondaryIndexKind])

-- FIXME:
-- Note: Think of naming consistency 'get*' vs 'schema*' etc.
-- seems that actual usage would seem weird with 'get*':
--
--   send $ getCreateTable proxy
--
-- vs:
--
--   send $ schemaCreateTable proxy
--


type instance DynamoName   (Table n a t s is)   = n
type instance HasAttribute (Table n a t s is) b = HasAttributes a (Attribute b)


-- FIXME: maybe call this converge table
-- since it takes the description and converges torwards the schema.
-- consider argument order.
--
-- Add warnings about destructive updates to indexes and attribute definitions
-- Add caveats about table name, and key definitions.

-- | Get the difference between a table schema and a remote 'TableDescription'
-- obtained via 'Network.AWS.DynamoDB.DescribeTable.describeTable', as an
-- 'Network.AWS.DynamoDB.UpdateTable.updateTable' request.
-- getUpdateTable :: DynamoTable a
--                => Proxy a
--                -> TableDescription
--                -> UpdateTable
-- getUpdateTable (getCreateTable -> a) (fromTableDescription -> b) =
--   where
--     update =
--         updateTable
--             & utTableName .~ b ^. ctTableName
--             & utAttributeDefinitions
--             & utProvisionedThroughput
--             & utStreamSpecification
--             & utGlobalSecondaryIndexUpdates


  -- updates can consist of;
  --     - attribute definitions
  --     - provisioned throughput
  --     - global secondary index updates
  --     - local secondary index updates
  --     - stream specificatoin

-- fromTableDescription :: TableDescription -> CreateTable
-- fromTableDescription x =
--     createTable
--         & ctTableName              .~ tdTableName              x
--         & ctKeySchema              .~ tdKeySchema              x
--         & ctAttributeDefinitions   .~ tdAttributeDefinitions   x
--         & ctGlobalSecondaryIndexes .~ tdGlobalSecondaryIndexes x
--         & ctLocalSecondaryIndexes  .~ tdLocalSecondaryIndexes  x
--         & ctStreamSpecification    .~ tdStreamSpecification    x
--         & ctProvisionedThroughput  .~ tdProvisionedThroughput  x

class ( DynamoAttributes a
      , DynamoKeys       a
      , DynamoThroughput a
      , DynamoStreaming  a
      , DynamoIndexes    a
      ) => DynamoTable a where
    -- | Get the DynamoDB 'CreateTable' configuration.
    getCreateTable :: Proxy a -> CreateTable

-- FIXME: remove this typeclass in favour of a simple function?

instance ( Table n a t s is ~  b
         , PartitionKeyOrder a
         , UniqueAttributes  a
         , DynamoAttributes    b
         , DynamoKeys          b
         , DynamoThroughput    b
         , DynamoStreaming     b
         , DynamoIndexes       b
         , KnownSymbol       n
         ) => DynamoTable (Table n a t s is) where
    getCreateTable _ =
        let p = Proxy :: Proxy b in
        createTable (getName p) (getKeys p)
           (getThroughput p)
                & ctStreamSpecification    ?~ getStreaming     p
                & ctAttributeDefinitions   .~ toList (getAttributes p)
                & ctGlobalSecondaryIndexes .~ getGlobalIndexes p
                & ctLocalSecondaryIndexes  .~ getLocalIndexes  p

instance ( UniqueAttributes a
         , DynamoAttributes a
         ) => DynamoAttributes (Table n a t s i) where
    getAttributes _ = getAttributes (Proxy :: Proxy a)

instance ( DynamoPartitionKey a
         ) => DynamoPartitionKey (Table n a t s i) where
    getPartitionKey _ = getPartitionKey (Proxy :: Proxy a)

instance ( DynamoSortKey Identity a
         ) => DynamoSortKey Identity (Table n a t s i) where
    getSortKey _ = getSortKey (Proxy :: Proxy a)

instance ( DynamoSortKey Maybe a
         ) => DynamoSortKey Maybe (Table n a t s i) where
    getSortKey _ = getSortKey (Proxy :: Proxy a)

instance DynamoThroughput t => DynamoThroughput (Table n a t s is) where
    getThroughput _ = getThroughput (Proxy :: Proxy t)

instance DynamoStreaming s => DynamoStreaming (Table n a t s is) where
    getStreaming _ = getStreaming (Proxy :: Proxy s)

instance ( DynamoIndexes (Schema a i)
         ) => DynamoIndexes (Table n a t s i) where
    getGlobalIndexes _ = getGlobalIndexes (Proxy :: Proxy (Schema a i))
    getLocalIndexes  _ = getLocalIndexes  (Proxy :: Proxy (Schema a i))

instance DynamoSerializer a => DynamoSerializer (Table n a t s is) where
    type Serialized   (Table n a t s is) = Serialized   a
    type Deserialized (Table n a t s is) = Deserialized a

    getSerializer   _ = getSerializer   (Proxy :: Proxy a)
    getDeserializer _ = getDeserializer (Proxy :: Proxy a)
