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
    , mkQueryObjectsInput
    -- ** Request lenses
    , qoiPipelineId
    , qoiQuery
    , qoiSphere
    , qoiMarker
    , qoiLimit

    -- * Response
    , QueryObjectsResponse
    -- ** Response lenses
    , qooIds
    , qooMarker
    , qooHasMoreResults
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'QueryObjects' request.
mkQueryObjectsInput :: Text -- ^ 'qoiPipelineId'
                    -> Text -- ^ 'qoiSphere'
                    -> QueryObjects
mkQueryObjectsInput p1 p2 = QueryObjects
    { _qoiPipelineId = p1
    , _qoiQuery = Nothing
    , _qoiSphere = p3
    , _qoiMarker = Nothing
    , _qoiLimit = Nothing
    }
{-# INLINE mkQueryObjectsInput #-}

data QueryObjects = QueryObjects
    { _qoiPipelineId :: Text
      -- ^ Identifier of the pipeline to be queried for object names.
    , _qoiQuery :: Maybe Query
      -- ^ Query that defines the objects to be returned. The Query object
      -- can contain a maximum of ten selectors. The conditions in the
      -- query are limited to top-level String fields in the object. These
      -- filters can be applied to components, instances, and attempts.
    , _qoiSphere :: Text
      -- ^ Specifies whether the query applies to components or instances.
      -- Allowable values: COMPONENT, INSTANCE, ATTEMPT.
    , _qoiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time
      -- you call QueryObjects, this value should be empty. As long as the
      -- action returns HasMoreResults as True, you can call QueryObjects
      -- again and pass the marker value from the response to retrieve the
      -- next set of results.
    , _qoiLimit :: Maybe Integer
      -- ^ Specifies the maximum number of object names that QueryObjects
      -- will return in a single call. The default value is 100.
    } deriving (Show, Generic)

-- | Identifier of the pipeline to be queried for object names.
qoiPipelineId :: Lens' QueryObjects (Text)
qoiPipelineId = lens _qoiPipelineId (\s a -> s { _qoiPipelineId = a })
{-# INLINE qoiPipelineId #-}

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
qoiQuery :: Lens' QueryObjects (Maybe Query)
qoiQuery = lens _qoiQuery (\s a -> s { _qoiQuery = a })
{-# INLINE qoiQuery #-}

-- | Specifies whether the query applies to components or instances. Allowable
-- values: COMPONENT, INSTANCE, ATTEMPT.
qoiSphere :: Lens' QueryObjects (Text)
qoiSphere = lens _qoiSphere (\s a -> s { _qoiSphere = a })
{-# INLINE qoiSphere #-}

-- | The starting point for the results to be returned. The first time you call
-- QueryObjects, this value should be empty. As long as the action returns
-- HasMoreResults as True, you can call QueryObjects again and pass the marker
-- value from the response to retrieve the next set of results.
qoiMarker :: Lens' QueryObjects (Maybe Text)
qoiMarker = lens _qoiMarker (\s a -> s { _qoiMarker = a })
{-# INLINE qoiMarker #-}

-- | Specifies the maximum number of object names that QueryObjects will return
-- in a single call. The default value is 100.
qoiLimit :: Lens' QueryObjects (Maybe Integer)
qoiLimit = lens _qoiLimit (\s a -> s { _qoiLimit = a })
{-# INLINE qoiLimit #-}

instance ToPath QueryObjects

instance ToQuery QueryObjects

instance ToHeaders QueryObjects

instance ToJSON QueryObjects

data QueryObjectsResponse = QueryObjectsResponse
    { _qooIds :: [Text]
      -- ^ A list of identifiers that match the query selectors.
    , _qooMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. As long as the
      -- action returns HasMoreResults as True, you can call QueryObjects
      -- again and pass the marker value from the response to retrieve the
      -- next set of results.
    , _qooHasMoreResults :: Maybe Bool
      -- ^ If True, there are more results that can be obtained by a
      -- subsequent call to QueryObjects.
    } deriving (Show, Generic)

-- | A list of identifiers that match the query selectors.
qooIds :: Lens' QueryObjectsResponse ([Text])
qooIds = lens _qooIds (\s a -> s { _qooIds = a })
{-# INLINE qooIds #-}

-- | The starting point for the results to be returned. As long as the action
-- returns HasMoreResults as True, you can call QueryObjects again and pass
-- the marker value from the response to retrieve the next set of results.
qooMarker :: Lens' QueryObjectsResponse (Maybe Text)
qooMarker = lens _qooMarker (\s a -> s { _qooMarker = a })
{-# INLINE qooMarker #-}

-- | If True, there are more results that can be obtained by a subsequent call
-- to QueryObjects.
qooHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qooHasMoreResults = lens _qooHasMoreResults (\s a -> s { _qooHasMoreResults = a })
{-# INLINE qooHasMoreResults #-}

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
