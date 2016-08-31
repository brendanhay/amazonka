{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |
-- Module      : Network.AWS.DynamoDB.Schema.Types
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Type classes for determing attribute scalar types and definitions suitable
-- for use with "Network.AWS.DynamoDB" operations.
module Network.AWS.DynamoDB.Schema.Attribute
    (
    -- * Scalar Types
      DynamoScalarType (..)

    -- * Attribute Definitions
    , DynamoAttributes (..)

    , AttributeKind (..)
    , PartitionKey
    , SortKey
    , Attribute
    , (:#)
    , (:::)
    , Schema

    -- * Invariants
    , PartitionKeyOrder
    , SortKeyOrder
    , UniqueAttributes
    , HasAttributes

    -- * Symbol Names
    , KnownSymbols (..)
    , symbolToText
    ) where

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

import qualified Data.Text as Text

-- | A DynamoDB table key or attribute.
--
-- This kind specifies the set of available keys and attributes.
-- The constructors are promoted to the type-level and unticked aliases are
-- also exported.
data AttributeKind
    = PartitionKey Symbol
    | SortKey      Symbol
    | Attribute    Symbol
    | AttributeKind :# AttributeKind
    | forall value . AttributeKind ::: value

type PartitionKey = 'PartitionKey
type SortKey      = 'SortKey
type Attribute    = 'Attribute

infixr 6 :#
infixr 7 :::

type a ::: b = a '::: b
type a :#  b = a ':#  b

-- | A type-level wrapper for passing the table's attribute schema
-- through type-class instance heads.
data Schema (schema :: AttributeKind) a

-- | Retrieve a type's corresponding DynamoDB scalar type.
--
-- Instances of this class are considered suitable for use as
-- a parameter to a table or index 'PartitionKey' and 'SortKey', but additional
-- considerations must be made about the value's uniformity, which affects
-- partition placement, data access, and provisioned throughput.
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GuidelinesForTables.html#GuidelinesForTables.UniformWorkload Table Guidelines - Uniform Workloads>.
class DynamoScalarType a where
    -- | Retrieve the 'ScalarAttributeType', which can be either a
    -- 'S' (string), 'N' (number), or 'B' (binary).
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalarType Text       where getScalarType = const S
instance DynamoScalarType Integer    where getScalarType = const N
instance DynamoScalarType ByteString where getScalarType = const B

-- | Retrieve a type's non-empty list of DynamoDB attribute definitions.
--
-- Instances are expected to enforce the invariant that resulting definition's
-- attribute names are unique.
--
-- /See:/ 'UniqueAttributes'.
class DynamoAttributes a where
    -- | Retrieve the non-empty list of 'AttributeDefinition's.
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance ( KnownSymbol n
         ) => DynamoAttributes (PartitionKey n) where
    getAttributes _ =
        getAttributes (Proxy :: Proxy (PartitionKey n ::: Text))

instance ( KnownSymbol      n
         , DynamoScalarType h
         ) => DynamoAttributes (PartitionKey n ::: h) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolToText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy h))

instance ( KnownSymbol n
         ) => DynamoAttributes (SortKey n) where
    getAttributes _ =
        getAttributes (Proxy :: Proxy (SortKey n ::: Text))

instance ( KnownSymbol      n
         , DynamoScalarType r
         ) => DynamoAttributes (SortKey n ::: r) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolToText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy r))

instance ( KnownSymbol n
         ) => DynamoAttributes (Attribute n) where
    getAttributes _ =
        getAttributes (Proxy :: Proxy (Attribute n ::: Text))

instance ( KnownSymbol      n
         , DynamoScalarType v
         ) => DynamoAttributes (Attribute n ::: v) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolToText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))

type family PartitionKeyOrder (a :: AttributeKind) :: Constraint where
    PartitionKeyOrder a =
        If (PartitionKeyOrder' (StripValues a))
           (() :: Constraint)
           (TypeError
               ('Text "PartitionKey must be specified first, then an optional\
                      \ SortKey, followed by any non-key Attributes:"
                ':$$: 'ShowType a))

-- | Test the 'PartitionKey' precedence invariants.
type family PartitionKeyOrder' (a :: AttributeKind) :: Bool where
    PartitionKeyOrder' (PartitionKey n)      = 'True
    PartitionKeyOrder' (PartitionKey n :# a) =
        SortKeyOrder' a || OnlyAttributes a
    PartitionKeyOrder' a                     = 'False

type family SortKeyOrder (a :: AttributeKind) :: Constraint where
    SortKeyOrder a =
        If (SortKeyOrder' (StripValues a))
           (() :: Constraint)
           (TypeError
               ('Text "SortKey must be specified first,\
                      \ followed by any non-key Attributes:"
                ':$$: 'ShowType a))

-- | Test the 'SortKey' precedence invariants.
type family SortKeyOrder' (a :: AttributeKind) :: Bool where
    SortKeyOrder' (SortKey n)      = 'True
    SortKeyOrder' (SortKey n :# a) = OnlyAttributes a
    SortKeyOrder' a                = 'False

-- | Enforce that all 'Attribute's named in 'a', are uniquely identified.
type family UniqueAttributes a :: Constraint where
    UniqueAttributes a =
        If (IsSet (AttributeNames a))
           (() :: Constraint)
           (TypeError
               ('Text "All Key and Attribute names must be unique:"
                ':$$: 'ShowType a))

-- | Enforce that all 'Attribute's named in 'b', exist in 'a'.
type family HasAttributes a b :: Constraint where
    HasAttributes a b =
        -- Remove all of 'a's attributes from 'b':
        If (Difference (AttributeNames b) (AttributeNames a) == '[])
           (() :: Constraint)
           (TypeError ('Text "Not all Keys or Attributes referenced in:"
                       ':$$: 'ShowType b
                       ':$$: 'Text "Are defined in the schema:"
                       ':$$: 'ShowType a))

-- | Test that only 'Attribute's and not keys are members of 'a'.
type family OnlyAttributes a :: Bool where
    OnlyAttributes (Attribute n) = 'True
    OnlyAttributes (a :# b)      = OnlyAttributes a && OnlyAttributes b
    OnlyAttributes (a ::: v)     = OnlyAttributes a
    OnlyAttributes a             = 'False

-- | Obtain a list of attribute names, verbatim.
type family AttributeNames a :: [Symbol] where
    AttributeNames (PartitionKey n) = '[n]
    AttributeNames (SortKey      n) = '[n]
    AttributeNames (Attribute    n) = '[n]
    AttributeNames (a :# b)         =
        AttributeNames a :++ AttributeNames b
    AttributeNames (a ::: v)        = AttributeNames a
    AttributeNames _                = '[]

type family StripValues (a :: AttributeKind) :: AttributeKind where
    StripValues (a :#  b) = StripValues a :# StripValues b
    StripValues (a ::: v) = StripValues a
    StripValues a         = a

type instance Cmp (a :: Symbol) (b :: Symbol) = CmpSymbol a b

-- | Check that 'a' is a set containing no duplicates.
type family IsSet a :: Bool where
    IsSet a = Sort a == Nub (Sort a)

-- | Does the element 'x' exist in the set 'xs'.
type family (∈) x xs :: Bool where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs

-- | All elements of set 'ys' removed from set 'xs'.
type family Difference xs ys where
    Difference '[]       ys        = '[]
    Difference xs       '[]        = xs
    Difference (x ': xs) (x ': ys) = Difference xs ys
    Difference (x ': xs) ys        =
        If (x ∈ ys)
           (Difference xs ys)
           (x ': Difference xs ys)

-- | Element 'x' removed from set 'xs'.
type family Remove x xs where
    Remove x '[]       = '[]
    Remove x (x ': ys) = ys
    Remove x (y ': ys) = y ': Remove x ys

class KnownSymbols a where
    symbolsToText :: Proxy a -> [Text]

instance KnownSymbols '[] where
    symbolsToText = const []

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
    symbolsToText _ =
        symbolToText (Proxy :: Proxy a) : symbolsToText (Proxy :: Proxy as)

instance KnownSymbol n => KnownSymbols (PartitionKey n) where
    symbolsToText _ = [symbolToText (Proxy :: Proxy n)]

instance KnownSymbol n => KnownSymbols (SortKey n) where
    symbolsToText _ = [symbolToText (Proxy :: Proxy n)]

instance KnownSymbol n => KnownSymbols (Attribute n) where
    symbolsToText _ = [symbolToText (Proxy :: Proxy n)]

instance (KnownSymbols a, KnownSymbols b) => KnownSymbols (a :# b) where
    symbolsToText _ =
        symbolsToText (Proxy :: Proxy a) ++ symbolsToText (Proxy :: Proxy b)

instance KnownSymbols a => KnownSymbols (a ::: v) where
    symbolsToText _ = symbolsToText (Proxy :: Proxy a)

symbolToText :: KnownSymbol n => proxy n -> Text
symbolToText = Text.pack . symbolVal
