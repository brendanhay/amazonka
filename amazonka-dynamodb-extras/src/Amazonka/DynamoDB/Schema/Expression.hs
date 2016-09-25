{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Amazonka.DynamoDB.Schema.Expression
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A specialized version of the "Amazonka.DynamoDB.Expression" language functions
-- and operators for use with table schemas defined by "Amazonka.DynamoDB.Schema".
module Amazonka.DynamoDB.Schema.Expression
    (
    -- * Key Condition Expressions
      KeyConditionExpression

    , partition
    , partitionFilter

    -- * Document Paths
    , Path

    , partitionKey
    , sortKey

    , path
    , name
    , index
    ) where

import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Item.Attribute
import Amazonka.DynamoDB.Schema.Attribute
import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Table

import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))

import qualified Amazonka.DynamoDB.Expression as Expr

-- | Specify a partition key condition.
partition :: DynamoPartitionKey a
          => Proxy a
             -- ^ A table schema.
          -> (Path Name -> Condition Hash Name v)
             -- ^ A partially applied partition key hash condition, such as @(=: "foo")@.
          -> KeyConditionExpression Name v
partition s f = Expr.partition (f (partitionKey s))

-- | Specify a partition key condition, and narrow the scope
-- by specifying a sort key condition.
partitionRange :: (DynamoPartitionKey a, DynamoSortKey Identity a)
               => Proxy a
                  -- ^ A table schema.
               -> (Path Name -> Condition Hash  Name v)
                  -- ^ A partially applied partition key hash condition, such as @(=: "foo")@.
               -> (Path Name -> Condition Range Name v)
                  -- ^ A partially applied sort key range condition, such as @(>: 123)@.
               -> KeyConditionExpression Name v
partitionRange s f g = Expr.partitionFilter (f (partitionKey s)) (g (sortKey s))

-- | Get a table schema's partition key as a 'Path' for use with the
-- "Amazonka.DynamoDB.Expression" language.
partitionKey :: DynamoPartitionKey a
             => Proxy a
                -- ^ A table schema.
             -> Path Name
partitionKey = pure . Name . getPartitionKey

-- | Get a table schema's sort key as a 'Path' for use with the
-- "Amazonka.DynamoDB.Expression" language.
sortKey :: DynamoSortKey Identity a
        => Proxy a
           -- ^ A table schema.
        -> Path Name
sortKey = pure . Name . runIdentity . getSortKey

-- | Create a document 'Path' for use with the "Amazonka.DynamoDB.Expression"
-- language, enforcing existence within the given table schema's attributes
-- for the given type.
path :: (IsTableAttribute a b, DynamoName b)
     => Proxy a
        -- ^ A table schema.
     -> Proxy b
        -- ^ The attribute type to select.
     -> Path Name
path _ = pure . Name . getName
