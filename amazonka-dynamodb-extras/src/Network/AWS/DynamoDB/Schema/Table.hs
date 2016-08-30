{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Schema.Table
    ( diffSchema
    , diffDescription

    , Table
    , getTableName
    , DynamoTable (..)

    , CreateTable
--    , create
    , UpdateTable
--    , update
    , DeleteTable
--    , delete

    ) where

import Control.Lens (view, (.~), (?~))

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import Network.AWS.DynamoDB.Schema.Attribute
import Network.AWS.DynamoDB.Schema.Index
import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Stream
import Network.AWS.DynamoDB.Schema.Throughput
import Network.AWS.DynamoDB.Schema.Types

-- Note: Think of naming consistency 'get*' vs 'schema*' etc.

getTableName :: DynamoTable a => Proxy a -> Text
getTableName = view ctTableName . getCreateTable

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
    getCreateTable _ =
        let p = Proxy :: Proxy b in
        createTable (symbolText (Proxy :: Proxy n))
           (getKeys p)
           (getThroughput p)
                & ctStreamSpecification    ?~ getStreaming     p
                & ctAttributeDefinitions   .~ toList (getAttributes p)
                & ctGlobalSecondaryIndexes .~ getGlobalIndexes p
                & ctLocalSecondaryIndexes  .~ getLocalIndexes  p
