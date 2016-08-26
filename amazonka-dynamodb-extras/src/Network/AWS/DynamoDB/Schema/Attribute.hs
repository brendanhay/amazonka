{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Schema.Attribute where

import Data.ByteString    (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Text          (Text)
import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Set      ((:++), Cmp, Nub, Sort)

import GHC.Exts     (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Types

class DynamoScalarType a where
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalarType Text       where getScalarType = const S
instance DynamoScalarType Integer    where getScalarType = const N
instance DynamoScalarType ByteString where getScalarType = const B

class DynamoAttributes a where
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance ( UniqueAttributes s
         , DynamoAttributes s
         ) => DynamoAttributes (Table n s o) where
    getAttributes _ = getAttributes (Proxy :: Proxy s)

instance ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance ( KnownSymbol      n
         , DynamoScalarType h
         ) => DynamoAttributes (PartitionKey n h) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy h))

instance ( KnownSymbol      n
         , DynamoScalarType r
         ) => DynamoAttributes (SortKey n r) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy r))

instance ( KnownSymbol      n
         , DynamoScalarType v
         ) => DynamoAttributes (Attribute n v) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))

-- | Check that only 'Attribute's are members of 'a'.
type family OnlyAttributes a :: Bool where
    OnlyAttributes (Attribute n v) = 'True
    OnlyAttributes (a :# b)        = OnlyAttributes a && OnlyAttributes b
    OnlyAttributes a               = 'False

-- | Assert that 'Attribute's in 'a', are uniquely identified.
type family UniqueAttributes a :: Constraint where
    UniqueAttributes a =
        If (IsSet (AttributeNames a)) (() :: Constraint)
           (TypeError
               ('Text "All Key and Attribute names must be unique:"
                ':$$: 'ShowType a))

-- | Assert that 'Attribute' references in 'a', exist in schema 's'.
type family HasAttributes s a :: Constraint where
    HasAttributes s a =
        -- Remove all of the schema 's' attributes from 'a':
        HasAttributes' s (Difference (AttributeNames a) (AttributeNames s))

type family HasAttributes' s a :: Constraint where
    HasAttributes' s '[] = ()
    HasAttributes' s  a  =
        TypeError ('Text "Keys or Attributes referenced by the names:"
                   ':$$: 'ShowType a
                   ':$$: 'Text "Are not defined in the schema:"
                   ':$$: 'ShowType s)

-- | Obtain all key and attribute names.
type family AttributeNames a :: [Symbol] where
    AttributeNames (PartitionKey      n h) = '[n]
    AttributeNames (SortKey           n r) = '[n]
    AttributeNames (Attribute         n v) = '[n]
    AttributeNames (IndexPartitionKey n)   = '[n]
    AttributeNames (IndexSortKey      n)   = '[n]
    AttributeNames (a :# b)                =
        AttributeNames a :++ AttributeNames b
    AttributeNames _                       = '[]

type instance Cmp (a :: Symbol) (b :: Symbol) = CmpSymbol a b

-- | Check that 'a' is a set containing no duplicates.
type family IsSet a :: Bool where
    IsSet a = Sort a == Nub (Sort a)
