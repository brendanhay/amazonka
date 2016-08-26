{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Schema.Table where

import Control.Lens (view, (.~))

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Proxy    (Proxy (..))
import Data.Text     (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Attribute
import Network.AWS.DynamoDB.Schema.Index
import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Throughput
import Network.AWS.DynamoDB.Schema.Types

class DynamoTable a where
    getTable :: Proxy a -> CreateTable

getTableName :: DynamoTable a => Proxy a -> Text
getTableName = view ctTableName . getTable

instance ( Table n s o    ~ t
         , DynamoAttributes t
         , DynamoKeys       t
         , DynamoIndexes    t
         , DynamoThroughput t
         , KnownSymbol      n
         ) => DynamoTable (Table n s o) where
    getTable _ =
        let p = Proxy :: Proxy t
          in createTable
               (symbolText (Proxy :: Proxy n))
               (getKeys p)
               (getThroughput p)
                   & ctAttributeDefinitions   .~ toList (getAttributes p)
                   & ctGlobalSecondaryIndexes .~ getGlobalIndexes p
                   & ctLocalSecondaryIndexes  .~ getLocalIndexes  p
