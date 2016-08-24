{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Mapper.Schema where

import Control.Lens ((?~), view)

import Data.List (nubBy)
import Data.ByteString              (ByteString)
import Data.Function                ((&), on)
import Data.HashMap.Strict          (HashMap)
import Data.List.NonEmpty           (NonEmpty (..))
import Data.Proxy                   (Proxy (..))
import Data.Semigroup               ((<>))
import Data.Text                    (Text)

import Data.Type.Bool      (If, type (||))
import Data.Type.Equality  (type (==))

import GHC.Exts (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper       hiding (Attribute)
import Network.AWS.DynamoDB.Mapper.Value (Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

type Example =
    Table "credentials" (ReadCapacity 1) (WriteCapacity 1)
       :# GlobalIndex "revision" (ReadCapacity 1) (WriteCapacity 1)
            (IndexHashKey "name" :# IndexRangeKey "revision")
       :# LocalIndex  "version"
            (IndexHashKey "name" :# IndexRangeKey "version")
       :# HashKey   "name"     Text
       :# RangeKey  "version"  Integer
       :# Attribute "revision" ByteString
       :# Field     "contents" ByteString

example :: Proxy Example
example = Proxy

-- Q: how to build a de/serializer for an index?
-- Q: how to build scan/query/etc based on the schema/index/projections?
-- Q: specifying index projections
-- Q: additional stream specification configuration parameters in the schema?


-- A: Order somewhat matters, Table > Index > Attributes
-- A: Non-unique indexes are currently filtered, and are not a compile time error.
-- A: Projections currently default to 'All'.
-- A: Field type is only used to add additional parameters to de/serializers.

infixr 8 :#, :*:

-- | A column specifier.
data a :# b

-- | A deserialized product.
data a :*: b = a :*: b
    deriving (Show)

data ReadCapacity  (n :: Nat)
data WriteCapacity (n :: Nat)

data HashKey       (n :: Symbol) v
data RangeKey      (n :: Symbol) v

data Table         (n :: Symbol) r w
data Attribute     (n :: Symbol) v
data Field         (n :: Symbol) v

data IndexHashKey  (n :: Symbol)
data IndexRangeKey (n :: Symbol)

data GlobalIndex   (n :: Symbol) ks r w
data LocalIndex    (n :: Symbol) ks

class DynamoScalar a where
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalar Text       where getScalarType = const S
instance DynamoScalar Integer    where getScalarType = const N
instance DynamoScalar ByteString where getScalarType = const B

class DynamoTable a where
    getTableName   :: Proxy a -> Text
    getCreateTable :: Proxy a -> CreateTable

instance KnownSymbol n => DynamoTable (Table n r w :# s) where
    getTableName   _ = symbolText (Proxy :: Proxy n)
    getCreateTable _

type family (∈) x xs where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs

type family (++) xs ys where
    (++) xs       '[]       = xs
    (++) '[]       ys        = ys
    (++) (x ': xs) ys = x ': (xs ++ ys)

type family Remove x xs where
    Remove x '[]       = '[]
    Remove x (x ': ys) = ys
    Remove x (y ': ys) = y ': Remove x ys

-- | This returns non-intersecting keys.
type family Intersect xs ys where
    Intersect '[]       ys        = '[]
    Intersect xs       '[]        = xs
    Intersect (x ': xs) (x ': ys) = Intersect xs ys
    Intersect (x ': xs) ys        =
          If (x ∈ ys)
             (Intersect xs (Remove x ys))
             '[x]

type family GetAttributeKeys xs where
    GetAttributeKeys (HashKey       n v) = '[n]
    GetAttributeKeys (RangeKey      n v) = '[n]
    GetAttributeKeys (Attribute     n v) = '[n]
    GetAttributeKeys (IndexHashKey  n)   = '[n]
    GetAttributeKeys (IndexRangeKey n)   = '[n]
    GetAttributeKeys (a :# b)            =
        GetAttributeKeys a ++ GetAttributeKeys b
    GetAttributeKeys _                   = '[]

type family HasAttributes ks a where
    HasAttributes ks a =
        MissingAttributes (Intersect (GetAttributeKeys ks) (GetAttributeKeys a)) a

type family MissingAttributes ks a :: Constraint where
    MissingAttributes '[]  a = ()
    MissingAttributes '[k] a =
           (TypeError
               ('Text "An attribute matching the name: "
                   ':<>: 'ShowType k
                   ':$$: 'Text "is not defined in the subsequent schema: "
                   ':<>: 'ShowType a))
    MissingAttributes ks   a =
           (TypeError
               ('Text "Attributes matching the names: "
                   ':<>: 'ShowType ks
                   ':$$: 'Text "are not defined in the subsequent schema: "
                   ':<>: 'ShowType a))

class DynamoIndexes a where
    getGlobalIndexes :: Proxy a -> [GlobalSecondaryIndex]
    getLocalIndexes  :: Proxy a -> [LocalSecondaryIndex]

    getGlobalIndexes = const []
    getLocalIndexes  = const []

instance {-# OVERLAPPABLE #-}
         ( DynamoIndexes a
         , DynamoIndexes b
         ) => DynamoIndexes (a :# b) where
    getGlobalIndexes _ = uniqueGlobalIndexes $
           getGlobalIndexes (Proxy :: Proxy a)
        <> getGlobalIndexes (Proxy :: Proxy b)

    getLocalIndexes  _ = uniqueLocalIndexes $
           getLocalIndexes (Proxy :: Proxy a)
        <> getLocalIndexes (Proxy :: Proxy b)

instance DynamoIndexes s => DynamoIndexes (Table n r w :# s) where
    getGlobalIndexes _ = getGlobalIndexes (Proxy :: Proxy s)
    getLocalIndexes  _ = getLocalIndexes  (Proxy :: Proxy s)

instance DynamoIndexes (HashKey   n v)
instance DynamoIndexes (RangeKey  n v)
instance DynamoIndexes (Attribute n v)
instance DynamoIndexes (Field     n)

instance ( DynamoIndexes    s
         , HasAttributes ks s
         , DynamoKeys    ks
         , KnownSymbol   n
         , KnownNat      r
         , KnownNat      w
         ) => DynamoIndexes (GlobalIndex n r w ks :# s) where
    getGlobalIndexes _ = uniqueGlobalIndexes $
        globalSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy ks))
            (projection & pProjectionType ?~ All)
            (getProvisionedThroughput (Proxy :: Proxy r) (Proxy :: Proxy w))
                : getGlobalIndexes (Proxy :: Proxy s)
    getLocalIndexes  _ = getLocalIndexes (Proxy :: Proxy s)

instance ( DynamoIndexes    s
         , HasAttributes ks s
         , DynamoKeys    ks
         , KnownSymbol   n
         ) => DynamoIndexes (LocalIndex n ks :# s) where
    getGlobalIndexes _ = getGlobalIndexes (Proxy :: Proxy s)
    getLocalIndexes  _ = uniqueLocalIndexes $
        localSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy ks))
            (projection & pProjectionType ?~ All)
                : getLocalIndexes (Proxy :: Proxy s)

-- FIXME: Migrate this to a type family.
uniqueGlobalIndexes :: [GlobalSecondaryIndex] -> [GlobalSecondaryIndex]
uniqueGlobalIndexes = nubBy (on (==) (view gsiIndexName))

-- FIXME: Migrate this to a type family.
uniqueLocalIndexes :: [LocalSecondaryIndex] -> [LocalSecondaryIndex]
uniqueLocalIndexes = nubBy (on (==) (view lsiIndexName))

getProvisionedThroughput :: (KnownNat r, KnownNat w)
                         => Proxy r
                         -> Proxy w
                         -> ProvisionedThroughput
getProvisionedThroughput r w =
    provisionedThroughput (fromIntegral (natVal r))
                          (fromIntegral (natVal w))

class DynamoAttributes a where
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance {-# OVERLAPPABLE #-}
         ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance DynamoAttributes s => DynamoAttributes (Table n r w :# s) where
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
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))

instance DynamoAttributes s => DynamoAttributes (LocalIndex n ks :# s) where
    getAttributes _ = getAttributes (Proxy :: Proxy s)

instance DynamoAttributes s => DynamoAttributes (s :# LocalIndex n ks) where
    getAttributes _ = getAttributes (Proxy :: Proxy s)

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
    getKeys _ = pure $ keySchemaElement (symbolText (Proxy :: Proxy n)) Hash

instance KnownSymbol n => DynamoKeys (RangeKey n v) where
    getKeys _ = pure $ keySchemaElement (symbolText (Proxy :: Proxy n)) Range

instance KnownSymbol n => DynamoKeys (IndexHashKey n) where
    getKeys _ = pure $ keySchemaElement (symbolText (Proxy :: Proxy n)) Hash

instance KnownSymbol n => DynamoKeys (IndexRangeKey n) where
    getKeys _ = pure $ keySchemaElement (symbolText (Proxy :: Proxy n)) Range

instance DynamoKeys s => DynamoKeys (Attribute n v :# s) where
    getKeys _ = getKeys (Proxy :: Proxy s)

instance DynamoKeys s => DynamoKeys (s :# Attribute n v) where
    getKeys _ = getKeys (Proxy :: Proxy s)

instance DynamoKeys s => DynamoKeys (LocalIndex n ks :# s) where
    getKeys _ = getKeys (Proxy :: Proxy s)

instance DynamoKeys s => DynamoKeys (s :# LocalIndex n ks) where
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
