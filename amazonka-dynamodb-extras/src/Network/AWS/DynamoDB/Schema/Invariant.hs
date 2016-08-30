{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Schema.Invariant
    ( PartitionKeyOrder
    , SortKeyOrder

    , UniqueAttributes
    , HasAttributes
    ) where

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Set      ((:++), Cmp, Nub, Sort)

import GHC.Exts     (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB.Schema.Types

type family PartitionKeyOrder (a :: AttributeKind) :: Constraint where
    PartitionKeyOrder a =
        If (PartitionKeyOrder' a)
           (() :: Constraint)
           (TypeError
               ('Text "PartitionKey must be specified first, then an optional\
                      \ SortKey, followed by any non-key Attributes:"
                ':$$: 'ShowType a))

-- | Test the 'PartitionKey' precedence invariants.
type family PartitionKeyOrder' (a :: AttributeKind) :: Bool where
    PartitionKeyOrder' (PartitionKey n h)      = 'True
    PartitionKeyOrder' (PartitionKey n h :# a) =
        SortKeyOrder' a || OnlyAttributes a
    PartitionKeyOrder' a                       = 'False

type family SortKeyOrder (a :: AttributeKind) :: Constraint where
    SortKeyOrder a =
        If (SortKeyOrder' a)
           (() :: Constraint)
           (TypeError
               ('Text "SortKey must be specified first,\
                      \ followed by any non-key Attributes:"
                ':$$: 'ShowType a))

-- | Test the 'SortKey' precedence invariants.
type family SortKeyOrder' (a :: AttributeKind) :: Bool where
    SortKeyOrder' (SortKey n r)      = 'True
    SortKeyOrder' (SortKey n r :# a) = OnlyAttributes a
    SortKeyOrder' a                  = 'False

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
    OnlyAttributes (Attribute n v) = 'True
    OnlyAttributes (a :# b)        = OnlyAttributes a && OnlyAttributes b
    OnlyAttributes a               = 'False

-- | Obtain a list of attribute names, verbatim.
type family AttributeNames a :: [Symbol] where
    AttributeNames (PartitionKey      n h) = '[n]
    AttributeNames (SortKey           n r) = '[n]
    AttributeNames (Attribute         n v) = '[n]
    AttributeNames (a :# b)                =
        AttributeNames a :++ AttributeNames b
    AttributeNames _                       = '[]

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
