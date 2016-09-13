{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Amazonka.DynamoDB.Schema.Serialize where

import Amazonka.DynamoDB.Item
import Amazonka.DynamoDB.Schema.Attribute

import Data.HashMap.Strict (HashMap)
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)

import GHC.TypeLits

import qualified Data.HashMap.Strict as HashMap

infixr 8 :*:

-- | A deserialized product.
data a :*: b = a :*: b
    deriving (Show)

serialize :: DynamoSerializer a => Proxy a -> Serialized a
serialize p = getSerializer p mempty

deserialize :: DynamoSerializer a => Proxy a -> Either ItemError (Deserialized a)
deserialize p = getDeserializer p mempty

-- Q: how to build a de/serializer for an index?
-- Q: how to build scan/query/etc based on the schema/index/projections?
-- Q: specifying index projections
-- Q: additional stream specification configuration parameters in the schema?
-- Q: Field type is only used to add additional parameters to de/serializers - how to make it work with getKeys?
-- Q: Non-unique indexes are currently filtered, and are not a compile time error.

-- A: Type-class invariants are only checked at the top-level instance, ie. for 'Table'.

class DynamoSerializer a where
    type Serialized   a
    type Deserialized a

    getSerializer :: Proxy a
                  -> HashMap Text Value
                  -> Serialized a

    getDeserializer :: Proxy a
                    -> HashMap Text Value
                    -> Either ItemError (Deserialized a)

instance ( DynamoSerializer a
         , KnownSymbol      n
         , DynamoValue      h
         ) => DynamoSerializer (PartitionKey n ::: h :# a) where
    type Serialized (PartitionKey n ::: h :# a) =
         Serialized (Attribute    n ::: h :# a)

    type Deserialized (PartitionKey n ::: h :# a) =
         Deserialized (Attribute    n ::: h :# a)

    getSerializer   _ =
        getSerializer   (Proxy :: Proxy (Attribute n ::: h :# a))

    getDeserializer _ =
        getDeserializer (Proxy :: Proxy (Attribute n ::: h :# a))

instance ( KnownSymbol n
         , DynamoValue h
         ) => DynamoSerializer (PartitionKey n ::: h) where
    type Serialized (PartitionKey n ::: h) =
         Serialized (Attribute    n ::: h)

    type Deserialized (PartitionKey n ::: h) =
         Deserialized (Attribute    n ::: h)

    getSerializer   _ =
        getSerializer   (Proxy :: Proxy (Attribute n ::: h))

    getDeserializer _ =
        getDeserializer (Proxy :: Proxy (Attribute n ::: h))

instance ( DynamoSerializer a
         , KnownSymbol      n
         , DynamoValue      r
         ) => DynamoSerializer (SortKey n ::: r :# a) where
    type Serialized (SortKey   n ::: r :# a) =
         Serialized (Attribute n ::: r :# a)

    type Deserialized (SortKey   n ::: r :# a) =
         Deserialized (Attribute n ::: r :# a)

    getSerializer   _ =
        getSerializer   (Proxy :: Proxy (Attribute n ::: r :# a))

    getDeserializer _ =
        getDeserializer (Proxy :: Proxy (Attribute n ::: r :# a))

instance ( KnownSymbol n
         , DynamoValue r
         ) => DynamoSerializer (SortKey n ::: r) where
    type Serialized (SortKey   n ::: r) =
         Serialized (Attribute n ::: r)

    type Deserialized (SortKey   n ::: r) =
         Deserialized (Attribute n ::: r)

    getSerializer   _ =
        getSerializer   (Proxy :: Proxy (Attribute n ::: r))

    getDeserializer _ =
        getDeserializer (Proxy :: Proxy (Attribute n ::: r))

instance ( DynamoSerializer a
         , KnownSymbol      n
         , DynamoValue      v
         ) => DynamoSerializer (Attribute n ::: v :# a) where
    type Serialized   (Attribute n ::: v :# a) = v ->  Serialized   a
    type Deserialized (Attribute n ::: v :# a) = v :*: Deserialized a

    getSerializer _ m =
          getSerializer (Proxy :: Proxy a)
        . getSerializer (Proxy :: Proxy (Attribute n ::: v)) m

    getDeserializer _ m =
        (:*:) <$> getDeserializer (Proxy :: Proxy (Attribute n ::: v)) m
              <*> getDeserializer (Proxy :: Proxy a) m

instance ( KnownSymbol n
         , DynamoValue v
         ) => DynamoSerializer (Attribute n ::: v) where
    type Serialized   (Attribute n ::: v) = v -> HashMap Text Value
    type Deserialized (Attribute n ::: v) = v

    getSerializer _ m v =
        uncurry HashMap.insert (value (symbolToText (Proxy :: Proxy n)) v) m

    getDeserializer _ =
        parse (symbolToText (Proxy :: Proxy n))
