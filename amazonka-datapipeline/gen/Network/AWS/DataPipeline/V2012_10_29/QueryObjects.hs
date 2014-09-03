{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.QueryObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Queries a pipeline for the names of objects that match a specified set of
-- conditions. The objects returned by QueryObjects are paginated and then
-- filtered by the value you set for query. This means the action may return
-- an empty result set with a value set for marker. If HasMoreResults is set
-- to True, you should continue to call QueryObjects, passing in the returned
-- value for marker, until HasMoreResults returns False. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.QueryObjects Content-Length: 123 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "query": {"selectors": [ ] }, "sphere": "PO", "marker": "", "limit": 10}
-- x-amzn-RequestId: 14d704c1-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 72 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "ids":
-- ["@SayHello_1_2012-09-25T17:00:00"] }.
module Network.AWS.DataPipeline.V2012_10_29.QueryObjects
    (
    -- * Request
      QueryObjects
    -- ** Request constructor
    , queryObjects
    -- ** Request lenses
    , qoiPipelineId
    , qoiSphere
    , qoiLimit
    , qoiQuery
    , qoiMarker

    -- * Response
    , QueryObjectsResponse
    -- ** Response lenses
    , qooHasMoreResults
    , qooIds
    , qooMarker
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'QueryObjects' request.
queryObjects :: Text -- ^ 'qoiPipelineId'
             -> Text -- ^ 'qoiSphere'
             -> QueryObjects
queryObjects p1 p2 = QueryObjects
    { _qoiPipelineId = p1
    , _qoiSphere = p2
    , _qoiLimit = Nothing
    , _qoiQuery = Nothing
    , _qoiMarker = Nothing
    }

data QueryObjects = QueryObjects
    { _qoiPipelineId :: Text
      -- ^ Identifier of the pipeline to be queried for object names.
    , _qoiSphere :: Text
      -- ^ Specifies whether the query applies to components or instances.
      -- Allowable values: COMPONENT, INSTANCE, ATTEMPT.
    , _qoiLimit :: Maybe Integer
      -- ^ Specifies the maximum number of object names that QueryObjects
      -- will return in a single call. The default value is 100.
    , _qoiQuery :: Maybe Query
      -- ^ Query that defines the objects to be returned. The Query object
      -- can contain a maximum of ten selectors. The conditions in the
      -- query are limited to top-level String fields in the object. These
      -- filters can be applied to components, instances, and attempts.
    , _qoiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time
      -- you call QueryObjects, this value should be empty. As long as the
      -- action returns HasMoreResults as True, you can call QueryObjects
      -- again and pass the marker value from the response to retrieve the
      -- next set of results.
    } deriving (Show, Generic)

-- | Identifier of the pipeline to be queried for object names.
qoiPipelineId
    :: Functor f
    => (Text
    -> f (Text))
    -> QueryObjects
    -> f QueryObjects
qoiPipelineId f x =
    (\y -> x { _qoiPipelineId = y })
       <$> f (_qoiPipelineId x)
{-# INLINE qoiPipelineId #-}

-- | Specifies whether the query applies to components or instances. Allowable
-- values: COMPONENT, INSTANCE, ATTEMPT.
qoiSphere
    :: Functor f
    => (Text
    -> f (Text))
    -> QueryObjects
    -> f QueryObjects
qoiSphere f x =
    (\y -> x { _qoiSphere = y })
       <$> f (_qoiSphere x)
{-# INLINE qoiSphere #-}

-- | Specifies the maximum number of object names that QueryObjects will return
-- in a single call. The default value is 100.
qoiLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> QueryObjects
    -> f QueryObjects
qoiLimit f x =
    (\y -> x { _qoiLimit = y })
       <$> f (_qoiLimit x)
{-# INLINE qoiLimit #-}

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
qoiQuery
    :: Functor f
    => (Maybe Query
    -> f (Maybe Query))
    -> QueryObjects
    -> f QueryObjects
qoiQuery f x =
    (\y -> x { _qoiQuery = y })
       <$> f (_qoiQuery x)
{-# INLINE qoiQuery #-}

-- | The starting point for the results to be returned. The first time you call
-- QueryObjects, this value should be empty. As long as the action returns
-- HasMoreResults as True, you can call QueryObjects again and pass the marker
-- value from the response to retrieve the next set of results.
qoiMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> QueryObjects
    -> f QueryObjects
qoiMarker f x =
    (\y -> x { _qoiMarker = y })
       <$> f (_qoiMarker x)
{-# INLINE qoiMarker #-}

instance ToPath QueryObjects

instance ToQuery QueryObjects

instance ToHeaders QueryObjects

instance ToJSON QueryObjects

data QueryObjectsResponse = QueryObjectsResponse
    { _qooHasMoreResults :: Bool
      -- ^ If True, there are more results that can be obtained by a
      -- subsequent call to QueryObjects.
    , _qooIds :: [Text]
      -- ^ A list of identifiers that match the query selectors.
    , _qooMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. As long as the
      -- action returns HasMoreResults as True, you can call QueryObjects
      -- again and pass the marker value from the response to retrieve the
      -- next set of results.
    } deriving (Show, Generic)

-- | If True, there are more results that can be obtained by a subsequent call
-- to QueryObjects.
qooHasMoreResults
    :: Functor f
    => (Bool
    -> f (Bool))
    -> QueryObjectsResponse
    -> f QueryObjectsResponse
qooHasMoreResults f x =
    (\y -> x { _qooHasMoreResults = y })
       <$> f (_qooHasMoreResults x)
{-# INLINE qooHasMoreResults #-}

-- | A list of identifiers that match the query selectors.
qooIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> QueryObjectsResponse
    -> f QueryObjectsResponse
qooIds f x =
    (\y -> x { _qooIds = y })
       <$> f (_qooIds x)
{-# INLINE qooIds #-}

-- | The starting point for the results to be returned. As long as the action
-- returns HasMoreResults as True, you can call QueryObjects again and pass
-- the marker value from the response to retrieve the next set of results.
qooMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> QueryObjectsResponse
    -> f QueryObjectsResponse
qooMarker f x =
    (\y -> x { _qooMarker = y })
       <$> f (_qooMarker x)
{-# INLINE qooMarker #-}

instance FromJSON QueryObjectsResponse

instance AWSRequest QueryObjects where
    type Sv QueryObjects = DataPipeline
    type Rs QueryObjects = QueryObjectsResponse

    request = get
    response _ = jsonResponse

instance AWSPager QueryObjects where
    next rq rs
        | not (_qooHasMoreResults rs) = Nothing
        | otherwise = Just $ rq
            { _qoiMarker = _qooMarker rs
            }
