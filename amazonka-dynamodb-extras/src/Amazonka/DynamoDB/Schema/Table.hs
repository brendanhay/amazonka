{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Amazonka.DynamoDB.Schema.Table
    ( diffSchema
    , diffDescription

    , Table
    , DynamoTable (..)

    , CreateTable
--    , create
    , UpdateTable
--    , update
    , DeleteTable
--    , delete

    ) where

import Amazonka.DynamoDB.Schema.Attribute
import Amazonka.DynamoDB.Schema.Index
import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Stream
import Amazonka.DynamoDB.Schema.Throughput
import Amazonka.DynamoDB.Schema.Serialize

import Control.Lens ((.~), (?~))

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)

import GHC.TypeLits

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

-- Note: Think of naming consistency 'get*' vs 'schema*' etc.

-- getTableName :: DynamoTable a => Proxy a -> Text
-- getTableName = view ctTableName . getCreateTable

-- create :: DynamoTable a => Proxy a -> CreateTable
-- create = getCreateTable

-- | Get the differences between two 'Table' schemas as an 'UpdateTable' request.
diffSchema :: (DynamoTable a, DynamoTable b)
           => Proxy a
           -> Proxy b
           -> UpdateTable
diffSchema = undefined

-- | Get the differences between a 'TableDescription' and a 'Table' schema
-- as an 'UpdateTable' request.
diffDescription :: DynamoTable a
                => TableDescription
                -> Proxy a
                -> UpdateTable
diffDescription = undefined

-- update :: (DynamoTable a, DynamoTable b)
--        => Proxy a
--        -> Proxy b
-- Get old table description? Actually run operations?
--        -> UpdateTable
-- update a b = convert . getCreateTable
--   where
--     convert x =

--     old = getCreateTable a
--     new = getCreateTable b

class DynamoTable a where
    -- | Get the DynamoDB table name.
    getTableName   :: Proxy a -> Text

    -- | Get the DynamoDB 'CreateTable' configuration.
    getCreateTable :: Proxy a -> CreateTable

instance ( Table n a t s is ~ b
         , DynamoAttributes b
         , DynamoKeys       b
         , DynamoThroughput b
         , DynamoStreaming  b
         , DynamoIndexes    b
         , KnownSymbol      n
         ) => DynamoTable (Table n a t s is) where
    getTableName   _ = symbolToText (Proxy :: Proxy n)
    getCreateTable _ =
        let p = Proxy :: Proxy b in
        createTable (getTableName p)
           (getKeys p)
           (getThroughput p)
                & ctStreamSpecification    ?~ getStreaming     p
                & ctAttributeDefinitions   .~ toList (getAttributes p)
                & ctGlobalSecondaryIndexes .~ getGlobalIndexes p
                & ctLocalSecondaryIndexes  .~ getLocalIndexes  p

instance ( UniqueAttributes a
         , DynamoAttributes a
         ) => DynamoAttributes (Table n a t s i) where
    getAttributes _ = getAttributes (Proxy :: Proxy a)

instance ( UniqueAttributes  a
         , PartitionKeyOrder a
         , DynamoKeys        a
         ) => DynamoKeys (Table n a t s i) where
    getKeys _ = getKeys (Proxy :: Proxy a)

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
