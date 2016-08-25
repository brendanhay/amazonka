{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Mapper.Table where

import Control.Lens (view, (.~))

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe    (fromMaybe)
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper.Attributes
import Network.AWS.DynamoDB.Mapper.Indexes
import Network.AWS.DynamoDB.Mapper.Keys
import Network.AWS.DynamoDB.Mapper.Throughput
import Network.AWS.DynamoDB.Mapper.Types

class DynamoTable a where
    getTable :: Proxy a -> CreateTable

instance ( Table n s o    ~ t
         , DynamoAttributes t
         , DynamoKeys       t
         , DynamoIndexes    t
         , DynamoThroughput t
         , KnownSymbol      n
         ) => DynamoTable (Table n s o) where
    getTable _ = createTable name (getKeys p) throughput
        & ctAttributeDefinitions   .~ toList (getAttributes p)
        & ctGlobalSecondaryIndexes .~ getGlobalIndexes p
        & ctLocalSecondaryIndexes  .~ getLocalIndexes  p
      where
        name       = symbolText (Proxy :: Proxy n)
        throughput = fromMaybe (provisionedThroughput 1 1) (getThroughput p)

        p = Proxy :: Proxy t

getTableName :: DynamoTable a => Proxy a -> Text
getTableName = view ctTableName . getTable
