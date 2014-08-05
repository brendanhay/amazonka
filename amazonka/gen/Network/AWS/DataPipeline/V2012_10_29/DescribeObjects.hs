{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.DataPipeline.V2012_10_29.DescribeObjects where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeObjects' request.
describeObjects :: Text -- ^ '_doiPipelineId'
                -> [Text] -- ^ '_doiObjectIds'
                -> DescribeObjects
describeObjects p1 p2 = DescribeObjects
    { _doiPipelineId = p1
    , _doiObjectIds = p2
    , _doiEvaluateExpressions = Nothing
    , _doiMarker = Nothing
    }

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

makeLenses ''DescribeObjects

instance ToPath DescribeObjects

instance ToQuery DescribeObjects

instance ToHeaders DescribeObjects

instance ToJSON DescribeObjects

data DescribeObjectsResponse = DescribeObjectsResponse
    { _dooPipelineObjects :: [PipelineObject]
      -- ^ An array of object definitions that are returned by the call to
      -- DescribeObjects.
    , _dooHasMoreResults :: Bool
      -- ^ If True, there are more pages of results to return.
    , _dooMarker :: Maybe Text
      -- ^ The starting point for the next page of results. To view the next
      -- page of results, call DescribeObjects again with this marker
      -- value.
    } deriving (Show, Generic)

makeLenses ''DescribeObjectsResponse

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
