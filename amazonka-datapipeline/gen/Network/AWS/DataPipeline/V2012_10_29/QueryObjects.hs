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
    , mkQueryObjects
    -- ** Request lenses
    , qoPipelineId
    , qoQuery
    , qoSphere
    , qoMarker
    , qoLimit

    -- * Response
    , QueryObjectsResponse
    -- ** Response constructor
    , mkQueryObjectsResponse
    -- ** Response lenses
    , qorIds
    , qorMarker
    , qorHasMoreResults
    ) where

import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input for the QueryObjects action.
data QueryObjects = QueryObjects
    { _qoPipelineId :: Text
    , _qoQuery :: Maybe Query
    , _qoSphere :: Text
    , _qoMarker :: Maybe Text
    , _qoLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'QueryObjects' request.
mkQueryObjects :: Text -- ^ 'qoPipelineId'
               -> Text -- ^ 'qoSphere'
               -> QueryObjects
mkQueryObjects p1 p3 = QueryObjects
    { _qoPipelineId = p1
    , _qoQuery = Nothing
    , _qoSphere = p3
    , _qoMarker = Nothing
    , _qoLimit = Nothing
    }

-- | Identifier of the pipeline to be queried for object names.
qoPipelineId :: Lens' QueryObjects Text
qoPipelineId = lens _qoPipelineId (\s a -> s { _qoPipelineId = a })

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
qoQuery :: Lens' QueryObjects (Maybe Query)
qoQuery = lens _qoQuery (\s a -> s { _qoQuery = a })

-- | Specifies whether the query applies to components or instances. Allowable
-- values: COMPONENT, INSTANCE, ATTEMPT.
qoSphere :: Lens' QueryObjects Text
qoSphere = lens _qoSphere (\s a -> s { _qoSphere = a })

-- | The starting point for the results to be returned. The first time you call
-- QueryObjects, this value should be empty. As long as the action returns
-- HasMoreResults as True, you can call QueryObjects again and pass the marker
-- value from the response to retrieve the next set of results.
qoMarker :: Lens' QueryObjects (Maybe Text)
qoMarker = lens _qoMarker (\s a -> s { _qoMarker = a })

-- | Specifies the maximum number of object names that QueryObjects will return
-- in a single call. The default value is 100.
qoLimit :: Lens' QueryObjects (Maybe Integer)
qoLimit = lens _qoLimit (\s a -> s { _qoLimit = a })

instance ToPath QueryObjects

instance ToQuery QueryObjects

instance ToHeaders QueryObjects

instance ToJSON QueryObjects

-- | Contains the output from the QueryObjects action.
data QueryObjectsResponse = QueryObjectsResponse
    { _qorIds :: [Text]
    , _qorMarker :: Maybe Text
    , _qorHasMoreResults :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'QueryObjectsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkQueryObjectsResponse :: Bool -- ^ 'qorHasMoreResults'
                       -> QueryObjectsResponse
mkQueryObjectsResponse p3 = QueryObjectsResponse
    { _qorIds = mempty
    , _qorMarker = Nothing
    , _qorHasMoreResults = p3
    }

-- | A list of identifiers that match the query selectors.
qorIds :: Lens' QueryObjectsResponse [Text]
qorIds = lens _qorIds (\s a -> s { _qorIds = a })

-- | The starting point for the results to be returned. As long as the action
-- returns HasMoreResults as True, you can call QueryObjects again and pass
-- the marker value from the response to retrieve the next set of results.
qorMarker :: Lens' QueryObjectsResponse (Maybe Text)
qorMarker = lens _qorMarker (\s a -> s { _qorMarker = a })

-- | If True, there are more results that can be obtained by a subsequent call
-- to QueryObjects.
qorHasMoreResults :: Lens' QueryObjectsResponse Bool
qorHasMoreResults =
    lens _qorHasMoreResults (\s a -> s { _qorHasMoreResults = a })

instance FromJSON QueryObjectsResponse

instance AWSRequest QueryObjects where
    type Sv QueryObjects = DataPipeline
    type Rs QueryObjects = QueryObjectsResponse

    request = get
    response _ = jsonResponse

instance AWSPager QueryObjects where
    next rq rs
        | not (rs ^. qorHasMoreResults) = Nothing
        | otherwise = Just $
            rq & qoMarker .~ rs ^. qorMarker
