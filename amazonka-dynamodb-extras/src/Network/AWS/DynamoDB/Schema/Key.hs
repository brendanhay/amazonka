{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Schema.Key where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Type.Bool

import GHC.Exts     (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Attribute
import Network.AWS.DynamoDB.Schema.Types

import qualified Data.List.NonEmpty as NE

-- Assumes that 'KeyInvariants' has been checked, and holds.
unsafeGetPartitionKey :: DynamoKeys a => Proxy a -> KeySchemaElement
unsafeGetPartitionKey = NE.head . getKeys

class DynamoKeys a where
    getKeys :: Proxy a -> NonEmpty KeySchemaElement

-- The current instance heads are specified in a total sense, without use of
-- nested 'DynamoKeys' instances to avoid any overlap, and to be clear about
-- the use of 'NonEmpty'.
--
-- This requires the 'KeyInvariants' constraint to be satisfied first.
instance ( KnownSymbol   n
         , KeyInvariants s
         , DynamoKeys    s
         ) => DynamoKeys (Table n s o) where
    getKeys _ = getKeys (Proxy :: Proxy s)

instance ( KnownSymbol n
         ) => DynamoKeys (PartitionKey n v) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (PartitionKey n h :# SortKey n' r) where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

instance ( KnownSymbol n
         ) => DynamoKeys (PartitionKey n h :# Attribute n' v) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (PartitionKey n h :# SortKey n' r :# a) where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

-- Global Indexes:

instance ( KnownSymbol n
         ) => DynamoKeys (IndexPartitionKey n) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (IndexPartitionKey n :# IndexSortKey n') where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (IndexPartitionKey n :# IndexSortKey n' :# a) where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

-- Local Indexes

instance ( KnownSymbol n
         ) => DynamoKeys (IndexSortKey n) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Range

instance ( KnownSymbol n
         ) => DynamoKeys (IndexSortKey n :# a) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Range

singletonKey :: KnownSymbol n => Proxy n -> KeyType -> NonEmpty KeySchemaElement
singletonKey p = pure . keySchemaElement (symbolText p)

-- | Invariants:
--   - A single PartitionKey must be specified first.
--   - A single SortKey may optionally be specified second.
--   - All key and attribute names must be unique.
type family KeyInvariants a :: Constraint where
    KeyInvariants (Table n s o) = KeyInvariants s
    KeyInvariants a             =
        If (KeyPrecedence a) (UniqueAttributes a)
           (TypeError
               ('Text "PartitionKey must be specified first, then an optional\
                      \ SortKey, followed by any non-key Attributes:"
                ':$$: 'ShowType a))

-- | Check the shape of the 'a' matches the key precedence invariants.
type family KeyPrecedence a :: Bool where
    KeyPrecedence (PartitionKey n h)                      = 'True
    KeyPrecedence (PartitionKey n h :# SortKey n' r)      = 'True
    KeyPrecedence (PartitionKey n h :# SortKey n' r :# a) = OnlyAttributes a
    KeyPrecedence (PartitionKey n h :# a)                 = OnlyAttributes a
