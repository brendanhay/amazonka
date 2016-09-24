{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Amazonka.DynamoDB.Schema.Index
    ( SecondaryIndexKind (..)
    , GlobalSecondaryIndex
    , LocalSecondaryIndex

    , DynamoIndexes (..)
    ) where

import Amazonka.DynamoDB.Schema.Attribute
import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Throughput

import Control.Lens ((?~), (^.))

import Data.Foldable         (toList)
import Data.Function         ((&))
import Data.Functor.Identity (Identity (..))
import Data.List             (nub, (\\))
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Proxy            (Proxy (..))
import Data.Semigroup        ((<>))
import Data.Text             (Text)

import GHC.TypeLits (KnownSymbol, Symbol)

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
         , DynamoKeys          a
         , PartitionKeyOrder   a
         , DynamoThroughput  t
         , KnownSymbol       n
         , KnownSymbols        a
         ) => DynamoIndexes (Schema s (GlobalSecondaryIndex n a t)) where
    getGlobalIndexes _ =
        pure $ globalSecondaryIndex
            (symbolToText (Proxy :: Proxy n))
            (getKeys (Proxy :: Proxy a))
            (project (nonKeyAttributes (Proxy :: Proxy a)
                                       (getKeys (Proxy :: Proxy a))))
            (getThroughput (Proxy :: Proxy t))

instance ( UniqueAttributes         a
         , HasAttributes          s a
         , SortKeyOrder             a
         , DynamoSortKey Identity   a
         , DynamoSortKey Maybe    s
         , KnownSymbols             a
         , KnownSymbol            n
         ) => DynamoIndexes (Schema s (LocalSecondaryIndex n a)) where
    getLocalIndexes _ =
        pure $ localSecondaryIndex
            (symbolToText (Proxy :: Proxy n))
            indexSortKey
            indexProjection
      where
        schemaSortKey = getSortKey (Proxy :: Proxy s)
        indexSortKey  = pure $
            keySchemaElement (runIdentity $ getSortKey (Proxy :: Proxy a)) Range

        -- Project 's' SortKey as a non-key attribute, if it exists.
        indexProjection =
            project . nub . maybe id (:) schemaSortKey $
                nonKeyAttributes (Proxy :: Proxy a) indexSortKey

project :: [Text] -> Projection
project []     =
    projection & pProjectionType ?~ KeysOnly
project (x:xs) =
    projection & pProjectionType ?~ Include
        & pNonKeyAttributes ?~ x :| xs

nonKeyAttributes :: forall f a. (Foldable f, KnownSymbols a)
                 => Proxy a
                 -> f KeySchemaElement
                 -> [Text]
nonKeyAttributes _ keys =
    symbolsToText (Proxy :: Proxy a)
        \\ map (^. kseAttributeName) (toList keys)
