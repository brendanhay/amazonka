{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.AWS.DynamoDB.Schema.Index where

import Control.Lens (view, (?~))

import Data.Function      ((&), on)
import Data.List          (nubBy)
import Data.List.NonEmpty ((<|))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Type.Bool

import GHC.Exts (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Throughput
import Network.AWS.DynamoDB.Schema.Types
import Network.AWS.DynamoDB.Schema.Attribute

import qualified Network.AWS.DynamoDB as Dynamo

-- | A type-level wrapper for passing the table's attribute schema
-- through type-class instance heads.
data Schema s a

class DynamoIndexes a where
    getGlobalIndexes :: Proxy a -> [Dynamo.GlobalSecondaryIndex]
    getLocalIndexes  :: Proxy a -> [Dynamo.LocalSecondaryIndex]

    getGlobalIndexes = const []
    getLocalIndexes  = const []

instance ( DynamoIndexes (Schema s o)
         ) => DynamoIndexes (Table n s o) where
    getGlobalIndexes _ =
        uniqueGlobalIndexes $ getGlobalIndexes (Proxy :: Proxy (Schema s o))

    getLocalIndexes  _ =
        uniqueLocalIndexes  $ getLocalIndexes  (Proxy :: Proxy (Schema s o))

instance ( DynamoIndexes (Schema s a)
         , DynamoIndexes (Schema s b)
         ) => DynamoIndexes (Schema s (a :# b)) where
    getGlobalIndexes _ =
           getGlobalIndexes (Proxy :: Proxy (Schema s a))
        <> getGlobalIndexes (Proxy :: Proxy (Schema s b))

    getLocalIndexes  _ =
           getLocalIndexes  (Proxy :: Proxy (Schema s a))
        <> getLocalIndexes  (Proxy :: Proxy (Schema s b))

-- Every global secondary index must have a partition key and can also
-- have an optional sort key.
-- The index key schema can be different from the table schema.
instance ( GlobalIndexInvariants s o
         , DynamoKeys              o
         , DynamoThroughput        o
         , KnownSymbol           n
         ) => DynamoIndexes (Schema s (GlobalSecondaryIndex n o)) where
    getGlobalIndexes _ =
        pure $ globalSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy o))
            (projection & pProjectionType ?~ All)
            (getThroughput (Proxy :: Proxy o))

-- Every local secondary index must meet the following conditions:
--
-- The partition key is the same as that of the source table.
-- The sort key consists of exactly one scalar attribute.
-- The sort key of the source table is projected into the index, where it acts as
-- a non-key attribute.
instance ( LocalIndexInvariants s o
         , DynamoKeys           s
         , DynamoKeys             o
         , KnownSymbol          n
         ) => DynamoIndexes (Schema s (LocalSecondaryIndex n o)) where
    getLocalIndexes _ =
        pure $ localSecondaryIndex
            (symbolText (Proxy :: Proxy n))
            (unsafeGetPartitionKey (Proxy :: Proxy s)
                <| getKeys (Proxy :: Proxy o))
            (projection & pProjectionType ?~ All)

instance DynamoIndexes (PartitionKey      n h)
instance DynamoIndexes (SortKey           n r)
instance DynamoIndexes (Attribute         n v)
instance DynamoIndexes (IndexPartitionKey n)
instance DynamoIndexes (IndexSortKey      n)
instance DynamoIndexes (Throughput        r w)
instance DynamoIndexes (Stream            v)
instance DynamoIndexes (Project           p)

-- FIXME: Migrate this to a type family.
uniqueGlobalIndexes :: [Dynamo.GlobalSecondaryIndex]
                    -> [Dynamo.GlobalSecondaryIndex]
uniqueGlobalIndexes = nubBy (on (==) (view gsiIndexName))

-- FIXME: Migrate this to a type family.
uniqueLocalIndexes :: [Dynamo.LocalSecondaryIndex]
                   -> [Dynamo.LocalSecondaryIndex]
uniqueLocalIndexes = nubBy (on (==) (view lsiIndexName))

-- | Invariants:
--   - A single IndexPartitionKey must be specified first.
--   - A single IndexSortKey may optionally be specified second.
type family GlobalIndexInvariants s a :: Constraint where
    GlobalIndexInvariants s a =
        If (GlobalKeyPrecedence a) (UniqueAttributes a ~ HasAttributes s a)
           (TypeError
               ('Text "IndexPartitionKey must be specified first, then an optional\
                      \ IndexSortKey, followed by any options:"
                ':$$: 'ShowType a))

type family GlobalKeyPrecedence a :: Bool where
    GlobalKeyPrecedence (IndexPartitionKey n)                         = 'True
    GlobalKeyPrecedence (IndexPartitionKey n :# IndexSortKey n')      = 'True
    GlobalKeyPrecedence (IndexPartitionKey n :# IndexSortKey n' :# a) =
        Not (HasIndexPartitionKey a || HasIndexSortKey a)
    GlobalKeyPrecedence (IndexPartitionKey n :# a)                    =
        Not (HasIndexPartitionKey a || HasIndexSortKey a)

-- | Invariants:
--   - A single IndexSortKey must be specified first.
type family LocalIndexInvariants s a :: Constraint where
    LocalIndexInvariants s a =
        If (LocalKeyPrecedence a) (UniqueAttributes a ~ HasAttributes s a)
           (TypeError
               ('Text "IndexSortKey must be specified first, followed by any options:"
                ':$$: 'ShowType a))

type family LocalKeyPrecedence a :: Bool where
    LocalKeyPrecedence (IndexSortKey n)      = 'True
    LocalKeyPrecedence (IndexSortKey n :# a) =
        Not (HasIndexPartitionKey a || HasIndexSortKey a)
    LocalKeyPrecedence a                     = 'False

type family HasIndexPartitionKey a :: Bool where
    HasIndexPartitionKey (IndexPartitionKey n) = 'True
    HasIndexPartitionKey (a :# b)              =
        HasIndexPartitionKey a || HasIndexPartitionKey b
    HasIndexPartitionKey a                     = 'False

type family HasIndexSortKey a :: Bool where
    HasIndexSortKey (IndexSortKey n) = 'True
    HasIndexSortKey (a :# b)         =
        HasIndexSortKey a || HasIndexSortKey b
    HasIndexSortKey a                = 'False
