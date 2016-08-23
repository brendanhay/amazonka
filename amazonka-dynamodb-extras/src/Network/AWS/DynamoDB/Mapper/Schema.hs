{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Network.AWS.DynamoDB.Mapper.Schema where

import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty  (NonEmpty (..), (<|))
import Data.Proxy          (Proxy (..))
import Data.Semigroup      ((<>))
import Data.Text           (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper       hiding (Attribute)
import Network.AWS.DynamoDB.Mapper.Value (Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

type Example =
       Table      "credentials"
    :# HashKey    "name"     Text
    :# RangeKey   "version"  Integer
    :# Attribute  "revision" ByteString
    :# Field      "contents" ByteString -- Field is only used for de/serialization.
    :# LocalIndex "revision"
        '[IndexHashKey "name", IndexRangeKey "revision"]
        (Include '["revision"])

-- Introduce a separate composition operator so the index can check the LHS
-- for the relevant attribute existence?
    -- :# GlobalIndex

-- Q: how to build a de/serializer for an index?
-- Q: how to build scan/query/etc based on the schema/index/projections?

example :: Proxy Example
example = Proxy

infixr 8 :#

-- | A column specifier.
data a :# b = a :# b
    deriving (Show)

infixr 8 :*:

-- | A deserialized product.
data a :*: b = a :*: b
    deriving (Show)

data HashKey  (n :: Symbol) v
data RangeKey (n :: Symbol) v

data Table     (n :: Symbol)
data Attribute (n :: Symbol) v
data Field     (n :: Symbol) v

data IndexHashKey  (n :: Symbol)
data IndexRangeKey (n :: Symbol)

data LocalIndex (n :: Symbol) (ks :: [*])

data All
data KeysOnly
data Include (keys :: [Symbol])

class DynamoScalar a where
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalar Text       where getScalarType = const S
instance DynamoScalar Integer    where getScalarType = const N
instance DynamoScalar ByteString where getScalarType = const B

class DynamoTable a where
    getTableName :: Proxy a -> Text

instance KnownSymbol n => DynamoTable (Table n :# s) where
    getTableName _ = symbolText (Proxy :: Proxy n)

class DynamoAttributes a where
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance {-# OVERLAPPABLE #-}
         ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance DynamoAttributes s => DynamoAttributes (Table n :# s) where
    getAttributes _ = getAttributes (Proxy :: Proxy s)

instance ( KnownSymbol  n
         , DynamoScalar v
         ) => DynamoAttributes (HashKey n v) where
    getAttributes _ = getAttributes (Proxy :: Proxy (Attribute n v))

instance ( KnownSymbol  n
         , DynamoScalar v
         ) => DynamoAttributes (RangeKey n v) where
    getAttributes _ = getAttributes (Proxy :: Proxy (Attribute n v))

instance ( KnownSymbol  n
         , DynamoScalar v
         ) => DynamoAttributes (Attribute n v) where
    getAttributes _ = pure $
        attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))

class DynamoKeys a where
    getKeys :: Proxy a -> NonEmpty KeySchemaElement

instance {-# OVERLAPPABLE #-}
         ( DynamoKeys a
         , DynamoKeys b
         ) => DynamoKeys (a :# b) where
    getKeys _ =
           getKeys (Proxy :: Proxy a)
        <> getKeys (Proxy :: Proxy b)

instance KnownSymbol n => DynamoKeys (HashKey n v) where
    getKeys _ = pure $
        keySchemaElement
            (symbolText (Proxy :: Proxy n))
            Hash

instance KnownSymbol n => DynamoKeys (RangeKey n v) where
    getKeys _ = pure $
        keySchemaElement
            (symbolText (Proxy :: Proxy n))
            Range

instance DynamoKeys s => DynamoKeys (Attribute n v :# s) where
    getKeys _ = getKeys (Proxy :: Proxy s)

instance DynamoKeys s => DynamoKeys (s :# Attribute n v) where
    getKeys _ = getKeys (Proxy :: Proxy s)

class DynamoSchema a where
    type Serializer   a
    type Deserializer a

    serializer :: Proxy a
               -> HashMap Text Value
               -> Serializer a

    deserializer :: Proxy a
                 -> HashMap Text Value
                 -> Either ItemError (Deserializer a)

instance ( DynamoSchema s
         , KnownSymbol  n
         , DynamoValue  v
         )
        => DynamoSchema (RangeKey n v :# s) where
    type Serializer   (RangeKey n v :# s) = v -> Serializer s
    type Deserializer (RangeKey n v :# s) = v :*: Deserializer s

    serializer _ m =
          serializer (Proxy :: Proxy s)
        . serializer (Proxy :: Proxy (RangeKey n v)) m

    deserializer _ m =
        (:*:) <$> deserializer (Proxy :: Proxy (RangeKey n v)) m
              <*> deserializer (Proxy :: Proxy s) m

instance ( KnownSymbol n
         , DynamoValue v
         )
        => DynamoSchema (HashKey n v) where
    type Serializer   (HashKey n v) = v -> HashMap Text Value
    type Deserializer (HashKey n v) = v

    serializer   _ = serializer   (Proxy :: Proxy (Attribute n v))
    deserializer _ = deserializer (Proxy :: Proxy (Attribute n v))

instance ( KnownSymbol n
         , DynamoValue v
         )
        => DynamoSchema (RangeKey n v) where
    type Serializer   (RangeKey n v) = v -> HashMap Text Value
    type Deserializer (RangeKey n v) = v

    serializer   _ = serializer   (Proxy :: Proxy (Attribute n v))
    deserializer _ = deserializer (Proxy :: Proxy (Attribute n v))

instance ( KnownSymbol n
         , DynamoValue v
         )
        => DynamoSchema (Attribute n v) where
    type Serializer   (Attribute n v) = v -> HashMap Text Value
    type Deserializer (Attribute n v) = v

    serializer _ m v =
        uncurry HashMap.insert (attr (symbolText (Proxy :: Proxy n)) v) m

    deserializer _ =
        parse (symbolText (Proxy :: Proxy n))

symbolText :: KnownSymbol n => proxy n -> Text
symbolText = Text.pack . symbolVal
