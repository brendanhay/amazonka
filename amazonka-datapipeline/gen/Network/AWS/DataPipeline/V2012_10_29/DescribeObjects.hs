{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.DescribeObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the object definitions for a set of objects associated with the
-- pipeline. Object definitions are composed of a set of fields that define
-- the properties of the object. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DescribeObjects
-- Content-Length: 98 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "objectIds": ["Schedule"], "evaluateExpressions":
-- true} x-amzn-RequestId: 4c18ea5d-0777-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 1488 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "pipelineObjects": [ {"fields": [
-- {"key": "startDateTime", "stringValue": "2012-12-12T00:00:00"}, {"key":
-- "parent", "refValue": "Default"}, {"key": "@sphere", "stringValue":
-- "COMPONENT"}, {"key": "type", "stringValue": "Schedule"}, {"key": "period",
-- "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-12-21T18:00:00"}, {"key": "@version", "stringValue": "1"}, {"key":
-- "@status", "stringValue": "PENDING"}, {"key": "@pipelineId", "stringValue":
-- "df-06372391ZG65EXAMPLE"} ], "id": "Schedule", "name": "Schedule"} ] }.
module Network.AWS.DataPipeline.V2012_10_29.DescribeObjects
    (
    -- * Request
      DescribeObjects
    -- ** Request constructor
    , mkDescribeObjectsInput
    -- ** Request lenses
    , doiPipelineId
    , doiObjectIds
    , doiEvaluateExpressions
    , doiMarker

    -- * Response
    , DescribeObjectsResponse
    -- ** Response lenses
    , dooPipelineObjects
    , dooMarker
    , dooHasMoreResults
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeObjects' request.
mkDescribeObjectsInput :: Text -- ^ 'doiPipelineId'
                       -> [Text] -- ^ 'doiObjectIds'
                       -> DescribeObjects
mkDescribeObjectsInput p1 p2 = DescribeObjects
    { _doiPipelineId = p1
    , _doiObjectIds = p2
    , _doiEvaluateExpressions = Nothing
    , _doiMarker = Nothing
    }
{-# INLINE mkDescribeObjectsInput #-}

data DescribeObjects = DescribeObjects
    { _doiPipelineId :: Text
      -- ^ Identifier of the pipeline that contains the object definitions.
    , _doiObjectIds :: [Text]
      -- ^ Identifiers of the pipeline objects that contain the definitions
      -- to be described. You can pass as many as 25 identifiers in a
      -- single call to DescribeObjects.
    , _doiEvaluateExpressions :: Maybe Bool
      -- ^ Indicates whether any expressions in the object should be
      -- evaluated when the object descriptions are returned.
    , _doiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time
      -- you call DescribeObjects, this value should be empty. As long as
      -- the action returns HasMoreResults as True, you can call
      -- DescribeObjects again and pass the marker value from the response
      -- to retrieve the next set of results.
    } deriving (Show, Generic)

-- | Identifier of the pipeline that contains the object definitions.
doiPipelineId :: Lens' DescribeObjects (Text)
doiPipelineId = lens _doiPipelineId (\s a -> s { _doiPipelineId = a })
{-# INLINE doiPipelineId #-}

-- | Identifiers of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- DescribeObjects.
doiObjectIds :: Lens' DescribeObjects ([Text])
doiObjectIds = lens _doiObjectIds (\s a -> s { _doiObjectIds = a })
{-# INLINE doiObjectIds #-}

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
doiEvaluateExpressions :: Lens' DescribeObjects (Maybe Bool)
doiEvaluateExpressions = lens _doiEvaluateExpressions (\s a -> s { _doiEvaluateExpressions = a })
{-# INLINE doiEvaluateExpressions #-}

-- | The starting point for the results to be returned. The first time you call
-- DescribeObjects, this value should be empty. As long as the action returns
-- HasMoreResults as True, you can call DescribeObjects again and pass the
-- marker value from the response to retrieve the next set of results.
doiMarker :: Lens' DescribeObjects (Maybe Text)
doiMarker = lens _doiMarker (\s a -> s { _doiMarker = a })
{-# INLINE doiMarker #-}

instance ToPath DescribeObjects

instance ToQuery DescribeObjects

instance ToHeaders DescribeObjects

instance ToJSON DescribeObjects

data DescribeObjectsResponse = DescribeObjectsResponse
    { _dooPipelineObjects :: [PipelineObject]
      -- ^ An array of object definitions that are returned by the call to
      -- DescribeObjects.
    , _dooMarker :: Maybe Text
      -- ^ The starting point for the next page of results. To view the next
      -- page of results, call DescribeObjects again with this marker
      -- value.
    , _dooHasMoreResults :: Maybe Bool
      -- ^ If True, there are more pages of results to return.
    } deriving (Show, Generic)

-- | An array of object definitions that are returned by the call to
-- DescribeObjects.
dooPipelineObjects :: Lens' DescribeObjectsResponse ([PipelineObject])
dooPipelineObjects = lens _dooPipelineObjects (\s a -> s { _dooPipelineObjects = a })
{-# INLINE dooPipelineObjects #-}

-- | The starting point for the next page of results. To view the next page of
-- results, call DescribeObjects again with this marker value.
dooMarker :: Lens' DescribeObjectsResponse (Maybe Text)
dooMarker = lens _dooMarker (\s a -> s { _dooMarker = a })
{-# INLINE dooMarker #-}

-- | If True, there are more pages of results to return.
dooHasMoreResults :: Lens' DescribeObjectsResponse (Maybe Bool)
dooHasMoreResults = lens _dooHasMoreResults (\s a -> s { _dooHasMoreResults = a })
{-# INLINE dooHasMoreResults #-}

instance FromJSON DescribeObjectsResponse

instance AWSRequest DescribeObjects where
    type Sv DescribeObjects = DataPipeline
    type Rs DescribeObjects = DescribeObjectsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeObjects where
    next rq rs
        | not (_dooHasMoreResults rs) = Nothing
        | otherwise = Just $ rq
            { _doiMarker = _dooMarker rs
            }
