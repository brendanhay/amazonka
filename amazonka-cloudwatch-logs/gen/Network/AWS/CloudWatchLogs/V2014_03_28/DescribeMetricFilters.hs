{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DescribeMetricFilters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name. By
-- default, this operation returns up to 50 metric filters. If there are more
-- metric filters to list, the response would contain a nextToken value in the
-- response body. You can also limit the number of metric filters returned in
-- the response by specifying the limit parameter in the request. List the
-- metric filters associated with a log group The following is an example of a
-- DescribeMetricFilters request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeMetricFilters { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "metricFilters": [ {
-- "creationTime": 1396224000000, "filterName": "exampleFilterName",
-- "filterPattern": "[ip, identity, user_id, timestamp, request, status_code,
-- size]", "metricTransformations": [ { "metricValue": "$size",
-- "metricNamespace": "MyApp", "metricName": "Volume" }, { "metricValue": "1",
-- "metricNamespace": "MyApp", "metricName": "RequestCount" } ] } ] }.
module Network.AWS.CloudWatchLogs.V2014_03_28.DescribeMetricFilters
    (
    -- * Request
      DescribeMetricFilters
    -- ** Request constructor
    , mkDescribeMetricFiltersRequest
    -- ** Request lenses
    , dmfsLogGroupName
    , dmfsFilterNamePrefix
    , dmfsNextToken
    , dmfsLimit

    -- * Response
    , DescribeMetricFiltersResponse
    -- ** Response lenses
    , dmftMetricFilters
    , dmftNextToken
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMetricFilters' request.
mkDescribeMetricFiltersRequest :: Text -- ^ 'dmfsLogGroupName'
                               -> DescribeMetricFilters
mkDescribeMetricFiltersRequest p1 = DescribeMetricFilters
    { _dmfsLogGroupName = p1
    , _dmfsFilterNamePrefix = Nothing
    , _dmfsNextToken = Nothing
    , _dmfsLimit = Nothing
    }
{-# INLINE mkDescribeMetricFiltersRequest #-}

data DescribeMetricFilters = DescribeMetricFilters
    { _dmfsLogGroupName :: Text
    , _dmfsFilterNamePrefix :: Maybe Text
      -- ^ The name of the metric filter.
    , _dmfsNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous DescribeMetricFilters request.
    , _dmfsLimit :: Maybe Integer
      -- ^ The maximum number of items returned in the response. If you
      -- don't specify a value, the request would return up to 50 items.
    } deriving (Show, Generic)

dmfsLogGroupName :: Lens' DescribeMetricFilters (Text)
dmfsLogGroupName = lens _dmfsLogGroupName (\s a -> s { _dmfsLogGroupName = a })
{-# INLINE dmfsLogGroupName #-}

-- | The name of the metric filter.
dmfsFilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmfsFilterNamePrefix = lens _dmfsFilterNamePrefix (\s a -> s { _dmfsFilterNamePrefix = a })
{-# INLINE dmfsFilterNamePrefix #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous
-- DescribeMetricFilters request.
dmfsNextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmfsNextToken = lens _dmfsNextToken (\s a -> s { _dmfsNextToken = a })
{-# INLINE dmfsNextToken #-}

-- | The maximum number of items returned in the response. If you don't specify
-- a value, the request would return up to 50 items.
dmfsLimit :: Lens' DescribeMetricFilters (Maybe Integer)
dmfsLimit = lens _dmfsLimit (\s a -> s { _dmfsLimit = a })
{-# INLINE dmfsLimit #-}

instance ToPath DescribeMetricFilters

instance ToQuery DescribeMetricFilters

instance ToHeaders DescribeMetricFilters

instance ToJSON DescribeMetricFilters

data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse
    { _dmftMetricFilters :: [MetricFilter]
    , _dmftNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    } deriving (Show, Generic)

dmftMetricFilters :: Lens' DescribeMetricFiltersResponse ([MetricFilter])
dmftMetricFilters = lens _dmftMetricFilters (\s a -> s { _dmftMetricFilters = a })
{-# INLINE dmftMetricFilters #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
dmftNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmftNextToken = lens _dmftNextToken (\s a -> s { _dmftNextToken = a })
{-# INLINE dmftNextToken #-}

instance FromJSON DescribeMetricFiltersResponse

instance AWSRequest DescribeMetricFilters where
    type Sv DescribeMetricFilters = CloudWatchLogs
    type Rs DescribeMetricFilters = DescribeMetricFiltersResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeMetricFilters where
    next rq rs = (\x -> rq { _dmfsNextToken = Just x })
        <$> (_dmftNextToken rs)
