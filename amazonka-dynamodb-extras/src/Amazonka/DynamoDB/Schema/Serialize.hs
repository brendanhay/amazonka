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

import GHC.TypeLits (KnownSymbol)

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

instance ( KnownSymbol (DynamoAttributeName h)
         , DynamoValue h
         , DynamoSerializer a
         ) => DynamoSerializer (PartitionKey h :# a) where
    type Serialized   (PartitionKey h :# a) = Serialized   (Attribute h :# a)
    type Deserialized (PartitionKey h :# a) = Deserialized (Attribute h :# a)

    getSerializer   _ = getSerializer   (Proxy :: Proxy (Attribute h :# a))
    getDeserializer _ = getDeserializer (Proxy :: Proxy (Attribute h :# a))

instance ( KnownSymbol (DynamoAttributeName h)
         , DynamoValue h
         ) => DynamoSerializer (PartitionKey h) where
    type Serialized   (PartitionKey h) = Serialized  (Attribute  h)
    type Deserialized (PartitionKey h) = Deserialized (Attribute h)

    getSerializer   _ = getSerializer   (Proxy :: Proxy (Attribute h))
    getDeserializer _ = getDeserializer (Proxy :: Proxy (Attribute h))

instance ( KnownSymbol (DynamoAttributeName r)
         , DynamoValue r
         , DynamoSerializer a
         ) => DynamoSerializer (SortKey r :# a) where
    type Serialized   (SortKey r :# a) = Serialized   (Attribute r :# a)
    type Deserialized (SortKey r :# a) = Deserialized (Attribute r :# a)

    getSerializer   _ = getSerializer   (Proxy :: Proxy (Attribute r :# a))
    getDeserializer _ = getDeserializer (Proxy :: Proxy (Attribute r :# a))

instance ( KnownSymbol (DynamoAttributeName r)
         , DynamoValue r
         ) => DynamoSerializer (SortKey r) where
    type Serialized   (SortKey r) = Serialized   (Attribute r)
    type Deserialized (SortKey r) = Deserialized (Attribute r)

    getSerializer   _ = getSerializer   (Proxy :: Proxy (Attribute r))
    getDeserializer _ = getDeserializer (Proxy :: Proxy (Attribute r))

instance ( KnownSymbol (DynamoAttributeName v)
         , DynamoValue v
         , DynamoSerializer a
         ) => DynamoSerializer (Attribute v :# a) where
    type Serialized   (Attribute v :# a) = v ->  Serialized   a
    type Deserialized (Attribute v :# a) = v :*: Deserialized a

    getSerializer _ m =
          getSerializer (Proxy :: Proxy a)
        . getSerializer (Proxy :: Proxy (Attribute v)) m

    getDeserializer _ m =
        (:*:) <$> getDeserializer (Proxy :: Proxy (Attribute v)) m
              <*> getDeserializer (Proxy :: Proxy a) m

instance ( KnownSymbol (DynamoAttributeName v)
         , DynamoValue v
         ) => DynamoSerializer (Attribute v) where
    type Serialized   (Attribute v) = v -> HashMap Text Value
    type Deserialized (Attribute v) = v

    getSerializer _ m x =
         HashMap.insert (getName (Proxy :: Proxy v)) (toValue x) m

    getDeserializer _ =
        parse (getName (Proxy :: Proxy v))
