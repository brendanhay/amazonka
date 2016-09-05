{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Amazonka.DynamoDB.Schema.Index
    ( SecondaryIndexKind (..)
    , GlobalSecondaryIndex
    , LocalSecondaryIndex

    , DynamoIndexes (..)
    ) where

import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Throughput
import Amazonka.DynamoDB.Schema.Attribute

import Control.Lens (view, (?~), (^.))

import Data.Foldable      (toList, find)
import Data.Function      ((&))
import Data.List          ((\\), nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Text          (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import qualified Network.AWS.DynamoDB as Dynamo

data SecondaryIndexKind
    = GlobalSecondaryIndex Symbol AttributeKind ThroughputKind
    | LocalSecondaryIndex  Symbol AttributeKind

-- | Every global secondary index must have a partition key and can also
-- have an optional sort key.
-- The index key schema can be different from the table schema
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.html
type GlobalSecondaryIndex = 'GlobalSecondaryIndex

-- <http://docs.aws.amazon.com/amazondynamodb/latt:DynamoAttributesest/APIReference/API_GlobalSecondaryIndex.html#DDB-Type-GlobalSecondaryIndex-KeySchema GlobalSecondaryIndex>
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_LocalSecondaryIndex.html#DDB-Type-LocalSecondaryIndex-KeySchema LocalSecondaryIndex>

-- | Every local secondary index must meet the following conditions:
--
-- The partition key is the same as that of the source table.
-- The sort key consists of exactly one scalar attribute.
-- The sort key of the source table is projected into the index, where it acts as
-- a non-key attribute.
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LSI.html
type LocalSecondaryIndex = 'LocalSecondaryIndex

class DynamoIndexes a where
    getGlobalIndexes :: Proxy a -> [Dynamo.GlobalSecondaryIndex]
    getLocalIndexes  :: Proxy a -> [Dynamo.LocalSecondaryIndex]

    getGlobalIndexes = const []
    getLocalIndexes  = const []

instance DynamoIndexes (Schema s '[])

instance ( DynamoIndexes (Schema s i)
         , DynamoIndexes (Schema s is)
         ) => DynamoIndexes (Schema s (i ': is)) where
    getGlobalIndexes _ =
           getGlobalIndexes (Proxy :: Proxy (Schema s i))
        <> getGlobalIndexes (Proxy :: Proxy (Schema s is))

    getLocalIndexes  _ =
           getLocalIndexes  (Proxy :: Proxy (Schema s i))
        <> getLocalIndexes  (Proxy :: Proxy (Schema s is))

instance ( UniqueAttributes    a
         , HasAttributes     s a
         , PartitionKeyOrder   a
         , DynamoKeys          a
         , DynamoThroughput  t
         , KnownSymbol       n
         , KnownSymbols        a
         ) => DynamoIndexes (Schema s (GlobalSecondaryIndex n a t)) where
    getGlobalIndexes _ =
        pure $ globalSecondaryIndex
            (symbolToText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy a))
            (project (nonKeyAttributes (Proxy :: Proxy a)))
            (getThroughput (Proxy :: Proxy t))

instance ( UniqueAttributes    a
         , HasAttributes     s a
         , SortKeyOrder        a
         , DynamoKeys        s
         , DynamoKeys          a
         , KnownSymbols        a
         , KnownSymbol       n
         ) => DynamoIndexes (Schema s (LocalSecondaryIndex n a)) where
    getLocalIndexes _ =
        pure $ localSecondaryIndex
            (symbolToText (Proxy :: Proxy n))
            (getKeys    (Proxy :: Proxy a))
            indexProjection
      where
        -- Project 's' SortKey as a non-key attribute, if it exists.
        schemaKeys = getKeys (Proxy :: Proxy s)
        sortKey    =
            view kseAttributeName
                <$> find ((== Range) . view kseKeyType) schemaKeys

        indexProjection =
            project . nub . maybe id (:) sortKey $
                nonKeyAttributes (Proxy :: Proxy a)

project :: [Text] -> Projection
project []     =
    projection & pProjectionType ?~ KeysOnly
project (x:xs) =
    projection & pProjectionType ?~ Include
        & pNonKeyAttributes ?~ x :| xs

nonKeyAttributes :: forall a. (DynamoKeys a, KnownSymbols a)
                 => Proxy a
                 -> [Text]
nonKeyAttributes _ =
    let attrs = map (^. kseAttributeName) (toList (getKeys (Proxy :: Proxy a)))
        keys  = symbolsToText (Proxy :: Proxy a)
     in attrs \\ keys
