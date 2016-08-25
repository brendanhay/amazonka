{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.AWS.DynamoDB.Mapper.Schema where

import Data.HashMap.Strict (HashMap)
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB.Mapper
import Network.AWS.DynamoDB.Mapper.Types
import Network.AWS.DynamoDB.Mapper.Value (Value)

import qualified Data.HashMap.Strict as HashMap

-- type Example =
--     Table "credentials"
--          ( Attribute "name"     Text
--         :# Attribute "version"  Integer
--         :# Attribute "revision" ByteString
--         :# Attribute "contents" (Maybe ByteString)
--          )
--         :# Key "name" 'Hash
--         :# Throughput (ReadCapacity 1) (WriteCapacity 1)
--         :# Stream 'STVKeysOnly
--         :# GlobalIndex "revision" (Key "name" 'Hash :# Key "revision" 'Range)
--              ( Throughput (ReadCapacity 1) (WriteCapacity 1)
--             :# Project 'All
--              )
--         :# LocalIndex "version" (Key "name" 'Hash :# Key "version" 'Range)
--          )

-- example :: Proxy Example
-- example = Proxy

-- Q: how to build a de/serializer for an index?
-- Q: how to build scan/query/etc based on the schema/index/projections?
-- Q: specifying index projections
-- Q: additional stream specification configuration parameters in the schema?
-- Q: Field type is only used to add additional parameters to de/serializers - how to make it work with getKeys?

-- A: Order somewhat matters, Table > Index > Attributes
-- A: Non-unique indexes are currently filtered, and are not a compile time error.
-- A: Projections currently default to 'All'.

class DynamoSchema a where
    type Serializer   a
    type Deserializer a

    serializer :: Proxy a
               -> HashMap Text Value
               -> Serializer a

    deserializer :: Proxy a
                 -> HashMap Text Value
                 -> Either ItemError (Deserializer a)

instance DynamoSchema s => DynamoSchema (Table n s o) where
    type Serializer   (Table n s o) = Serializer s
    type Deserializer (Table n s o) = Deserializer s

    serializer   _ = serializer   (Proxy :: Proxy s)
    deserializer _ = deserializer (Proxy :: Proxy s)

instance ( DynamoSchema s
         , KnownSymbol  n
         , DynamoValue  v
         ) => DynamoSchema (Attribute n v :# s) where
    type Serializer   (Attribute n v :# s) = v ->  Serializer   s
    type Deserializer (Attribute n v :# s) = v :*: Deserializer s

    serializer _ m =
          serializer (Proxy :: Proxy s)
        . serializer (Proxy :: Proxy (Attribute n v)) m

    deserializer _ m =
        (:*:) <$> deserializer (Proxy :: Proxy (Attribute n v)) m
              <*> deserializer (Proxy :: Proxy s) m

instance ( KnownSymbol n
         , DynamoValue v
         ) => DynamoSchema (Attribute n v) where
    type Serializer   (Attribute n v) = v -> HashMap Text Value
    type Deserializer (Attribute n v) = v

    serializer _ m v =
        uncurry HashMap.insert (attr (symbolText (Proxy :: Proxy n)) v) m

    deserializer _ =
        parse (symbolText (Proxy :: Proxy n))
