{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Amazonka.DynamoDB.Plan
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Extensions to 'Query' and 'Scan' requests to allow use of the
-- "Amazonka.DynamoDB.Expression" langauge.
module Amazonka.DynamoDB.Plan
    (
    -- * Usage
    -- $usage

      QueryPlan
    , compileQuery

    , ScanPlan
    , compileScan

    , Plan

    , HasKeyCondition (..)
    , HasProjection   (..)
    , HasFilter       (..)
    ) where

import Network.AWS.DynamoDB

import Amazonka.DynamoDB.Expression.Compile
import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Expression.Placeholder
import Amazonka.DynamoDB.Item.Value             (Value, getValue)

import Control.Lens                     (Lens, lens)
import Control.Lens.Operators           ((.~))
import Control.Monad.Trans.State.Strict (runState)

import Data.Function ((&))

import Network.AWS.Data.Text (toText)

-- |
data QueryPlan = QueryPlan
    { _queryKey    :: Maybe (KeyConditionExpression Name Value)
    , _queryProj   :: Maybe (ProjectionExpression   Name)
    , _queryFilter :: Maybe (ConditionExpression    Name Value)
    , _query       :: Query
    }

-- |
overrideQuery :: Query -> QueryPlan
overrideQuery = QueryPlan Nothing Nothing Nothing

-- |
compileQuery :: QueryPlan -> Query
compileQuery QueryPlan{..} = _query
    & qKeyConditionExpression    .~ fmap finalize k
    & qProjectionExpression      .~ fmap finalize p
    & qFilterExpression          .~ fmap finalize f
    & qExpressionAttributeNames  .~ fmap toText   (finalizeNames  ps)
    & qExpressionAttributeValues .~ fmap getValue (finalizeValues ps)
  where
    ((k, p, f), ps) =
        flip runState placeholders $
            (,,) <$> compileT      keyConditionExpression _queryKey
                 <*> compileNamesT projectionExpression   _queryProj
                 <*> compileT      conditionExpression    _queryFilter

-- |
data ScanPlan = ScanPlan
    { _scanProj   :: Maybe (ProjectionExpression Name)
    , _scanFilter :: Maybe (ConditionExpression  Name Value)
    , _scan       :: Scan
    }

-- |
overrideScan :: Scan -> ScanPlan
overrideScan = ScanPlan Nothing Nothing

-- |
compileScan :: ScanPlan -> Scan
compileScan ScanPlan{..} = _scan
    & sProjectionExpression      .~ fmap finalize p
    & sFilterExpression          .~ fmap finalize f
    & sExpressionAttributeNames  .~ fmap toText   (finalizeNames  ps)
    & sExpressionAttributeValues .~ fmap getValue (finalizeValues ps)
  where
    ((p, f), ps) =
        flip runState placeholders $
            (,) <$> compileNamesT projectionExpression _scanProj
                <*> compileT      conditionExpression  _scanFilter

-- |
type family Plan a

type instance Plan Query     = QueryPlan
type instance Plan QueryPlan = QueryPlan
type instance Plan Scan      = ScanPlan
type instance Plan ScanPlan  = ScanPlan

class HasKeyCondition a where
    pKeyCondition
        :: Lens a (Plan a) (Maybe (KeyConditionExpression Name Value)) (Maybe (KeyConditionExpression Name Value))

instance HasKeyCondition Query where
    pKeyCondition f = pKeyCondition f . overrideQuery

instance HasKeyCondition QueryPlan where
    pKeyCondition = lens _queryKey (\s a -> s { _queryKey = a })

class HasProjection a where
    pProjection
        :: Lens a (Plan a) (Maybe (ProjectionExpression Name)) (Maybe (ProjectionExpression Name))

instance HasProjection Query where
    pProjection f = pProjection f . overrideQuery

instance HasProjection QueryPlan where
    pProjection = lens _queryProj (\s a -> s { _queryProj = a })

instance HasProjection Scan where
    pProjection f = pProjection f . overrideScan

instance HasProjection ScanPlan where
    pProjection = lens _scanProj (\s a -> s { _scanProj = a })

class HasFilter a where
    pFilter
        :: Lens a (Plan a) (Maybe (ConditionExpression Name Value)) (Maybe (ConditionExpression Name Value))

instance HasFilter Query where
    pFilter f = pFilter f . overrideQuery

instance HasFilter QueryPlan where
    pFilter = lens _queryFilter (\s a -> s { _queryFilter = a })

instance HasFilter Scan where
    pFilter f = pFilter f . overrideScan

instance HasFilter ScanPlan where
    pFilter = lens _scanFilter (\s a -> s { _scanFilter = a })

-- $usage
--
-- For example, to create a typical 'Query' request, you would construct the query
-- as follows:
--
-- @
-- myQuery :: Query
-- myQuery = query "my-table-name"
--     & qIndexName      ?~ "my-index-name"
--     & qConsistentRead ?~ False
--     & qLimit          ?~ 25
-- @
--
-- To use the "Amazonka.DynamoDB.Expression" language:
--
-- @
-- myQuery :: QueryPlan
-- myQuery = query "my-table-name"
--     & qIndexName      ?~ "my-index-name"
--     & qConsistentRead ?~ False
--     & qLimit          ?~ 25
--     & pKeyCondition   ?~ partitionFilter (name "ProductId" =: "foo") (name "CategoryId" >: 123)
--     & pProjection     ?~ project (name "Title") <> project (index "RelatedItems" 2)
--     & pFilter         ?~ exists (name "Pictures" <> name "SideView")
-- @
--
-- Notice the use of the 'pKeyCondition', 'pProjection', and 'pFilter' lenses
-- changes the type from that of a 'Query', to an opaque 'QueryPlan'. This destroys
-- any information that was previously set for key conditions, projections,
-- and filters, as well as clearing any expression attribute names and value placeholders.
--
-- You can then compile the 'QueryPlan' back to a 'Query' suitable for use with 'send'
-- by using 'compileQuery':
--
-- @
-- myQuery :: Query
-- myQuery = query "my-table-name"
--     & qIndexName      ?~ "my-index-name"
--     & qConsistentRead ?~ False
--     & qLimit          ?~ 25
--     & pKeyCondition   ?~ partitionFilter (name "ProductId" =: "foo") (name "CategoryId" >: 123)
--     & pProjection     ?~ project (name "Title") <> project (index "RelatedItems" 2)
--     & pFilter         ?~ exists (name "Pictures" <> name "SideView")
--     & compileQuery
-- @
--
