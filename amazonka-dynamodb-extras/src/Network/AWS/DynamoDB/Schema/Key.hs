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

import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import Network.AWS.DynamoDB.Schema.Invariant
import Network.AWS.DynamoDB.Schema.Types

import qualified Data.List.NonEmpty as NE

-- Assumes that 'KeyInvariants' has been checked, and holds.
unsafeGetPartitionKey :: DynamoKeys a => Proxy a -> KeySchemaElement
unsafeGetPartitionKey = NE.head . getKeys

class DynamoKeys a where
    getKeys :: Proxy a -> NonEmpty KeySchemaElement

instance ( UniqueAttributes  a
         , PartitionKeyOrder a
         , DynamoKeys        a
         ) => DynamoKeys (Table n a t s i) where
    getKeys _ = getKeys (Proxy :: Proxy a)

instance ( KnownSymbol n
         ) => DynamoKeys (PartitionKey n h) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (PartitionKey n h :# SortKey n' r) where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

instance ( KnownSymbol n
         ) => DynamoKeys (PartitionKey n h :# Attribute n' v) where
    getKeys _ =
         singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         , KnownSymbol n'
         ) => DynamoKeys (PartitionKey n h :# SortKey n' r :# a) where
    getKeys _ =
           singletonKey (Proxy :: Proxy n)  Hash
        <> singletonKey (Proxy :: Proxy n') Range

instance ( KnownSymbol n
         ) => DynamoKeys (PartitionKey n h :# Attribute n' v :# a) where
    getKeys _ =
         singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         ) => DynamoKeys (SortKey n r) where
    getKeys _ = singletonKey (Proxy :: Proxy n) Hash

instance ( KnownSymbol n
         ) => DynamoKeys (SortKey n r :# a) where
    getKeys _ = singletonKey (Proxy :: Proxy n)  Hash

singletonKey :: KnownSymbol n => Proxy n -> KeyType -> NonEmpty KeySchemaElement
singletonKey p = pure . keySchemaElement (symbolText p)
