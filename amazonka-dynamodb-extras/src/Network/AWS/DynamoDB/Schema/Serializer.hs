{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Network.AWS.DynamoDB.Schema.Serializer where

import Data.HashMap.Strict (HashMap)
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB (ProjectionType (..), StreamViewType (..))

import Network.AWS.DynamoDB.Mapper.Item
import Network.AWS.DynamoDB.Mapper.Value
import Network.AWS.DynamoDB.Schema.Types

import qualified Data.HashMap.Strict as HashMap

type Example =
    Table "credentials"
        ( PartitionKey "name"     Text
       :# Attribute    "revision" Text
       :# Attribute    "contents" Text
       :# SortKey      "version"  Integer
        )

        ( Throughput (ReadCapacity 1) (WriteCapacity 1)

       :# Stream 'SVTKeysOnly

       :# GlobalSecondaryIndex "revision"
             ( IndexPartitionKey "name"
            :# IndexSortKey      "revision"
            :# Throughput (ReadCapacity 1) (WriteCapacity 1)
            :# Project 'All
             )

       :# LocalSecondaryIndex "version"
             ( IndexSortKey "contents"
            :# Project 'KeysOnly
             )
        )

example :: Proxy Example
example = Proxy

-- Q: how to build a de/serializer for an index?
-- Q: how to build scan/query/etc based on the schema/index/projections?
-- Q: specifying index projections
-- Q: additional stream specification configuration parameters in the schema?
-- Q: Field type is only used to add additional parameters to de/serializers - how to make it work with getKeys?
-- Q: Non-unique indexes are currently filtered, and are not a compile time error.

-- A: Type-class invariants are only checked at the top-level instance, ie. for 'Table'.

class DynamoSerializer a where
    type Serializer   a
    type Deserializer a

    serialize :: Proxy a
              -> HashMap Text Value
              -> Serializer a

    deserialize :: Proxy a
                -> HashMap Text Value
                -> Either ItemError (Deserializer a)

instance DynamoSerializer s => DynamoSerializer (Table n s o) where
    type Serializer   (Table n s o) = Serializer s
    type Deserializer (Table n s o) = Deserializer s

    serialize   _ = serialize   (Proxy :: Proxy s)
    deserialize _ = deserialize (Proxy :: Proxy s)

instance ( DynamoSerializer s
         , KnownSymbol      n
         , DynamoValue      v
         ) => DynamoSerializer (Attribute n v :# s) where
    type Serializer   (Attribute n v :# s) = v ->  Serializer   s
    type Deserializer (Attribute n v :# s) = v :*: Deserializer s

    serialize _ m =
          serialize (Proxy :: Proxy s)
        . serialize (Proxy :: Proxy (Attribute n v)) m

    deserialize _ m =
        (:*:) <$> deserialize (Proxy :: Proxy (Attribute n v)) m
              <*> deserialize (Proxy :: Proxy s) m

instance ( KnownSymbol n
         , DynamoValue v
         ) => DynamoSerializer (Attribute n v) where
    type Serializer   (Attribute n v) = v -> HashMap Text Value
    type Deserializer (Attribute n v) = v

    serialize _ m v =
        uncurry HashMap.insert (attr (symbolText (Proxy :: Proxy n)) v) m

    deserialize _ =
        parse (symbolText (Proxy :: Proxy n))
