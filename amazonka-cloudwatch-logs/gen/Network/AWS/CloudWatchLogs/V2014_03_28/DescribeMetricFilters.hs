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
    , mkDescribeMetricFilters
    -- ** Request lenses
    , dmf1LogGroupName
    , dmf1FilterNamePrefix
    , dmf1NextToken
    , dmf1Limit

    -- * Response
    , DescribeMetricFiltersResponse
    -- ** Response lenses
    , dmfrsMetricFilters
    , dmfrsNextToken
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeMetricFilters = DescribeMetricFilters
    { _dmf1LogGroupName :: Text
    , _dmf1FilterNamePrefix :: Maybe Text
    , _dmf1NextToken :: Maybe Text
    , _dmf1Limit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMetricFilters' request.
mkDescribeMetricFilters :: Text -- ^ 'dmf1LogGroupName'
                        -> DescribeMetricFilters
mkDescribeMetricFilters p1 = DescribeMetricFilters
    { _dmf1LogGroupName = p1
    , _dmf1FilterNamePrefix = Nothing
    , _dmf1NextToken = Nothing
    , _dmf1Limit = Nothing
    }
{-# INLINE mkDescribeMetricFilters #-}

dmf1LogGroupName :: Lens' DescribeMetricFilters Text
dmf1LogGroupName =
    lens _dmf1LogGroupName (\s a -> s { _dmf1LogGroupName = a })
{-# INLINE dmf1LogGroupName #-}

-- | The name of the metric filter.
dmf1FilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmf1FilterNamePrefix =
    lens _dmf1FilterNamePrefix (\s a -> s { _dmf1FilterNamePrefix = a })
{-# INLINE dmf1FilterNamePrefix #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous
-- DescribeMetricFilters request.
dmf1NextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmf1NextToken = lens _dmf1NextToken (\s a -> s { _dmf1NextToken = a })
{-# INLINE dmf1NextToken #-}

-- | The maximum number of items returned in the response. If you don't specify
-- a value, the request would return up to 50 items.
dmf1Limit :: Lens' DescribeMetricFilters (Maybe Integer)
dmf1Limit = lens _dmf1Limit (\s a -> s { _dmf1Limit = a })
{-# INLINE dmf1Limit #-}

instance ToPath DescribeMetricFilters

instance ToQuery DescribeMetricFilters

instance ToHeaders DescribeMetricFilters

instance ToJSON DescribeMetricFilters

data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse
    { _dmfrsMetricFilters :: [MetricFilter]
    , _dmfrsNextToken :: Maybe Text
    } deriving (Show, Generic)

dmfrsMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrsMetricFilters =
    lens _dmfrsMetricFilters (\s a -> s { _dmfrsMetricFilters = a })
{-# INLINE dmfrsMetricFilters #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
dmfrsNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmfrsNextToken = lens _dmfrsNextToken (\s a -> s { _dmfrsNextToken = a })
{-# INLINE dmfrsNextToken #-}

instance FromJSON DescribeMetricFiltersResponse

instance AWSRequest DescribeMetricFilters where
    type Sv DescribeMetricFilters = CloudWatchLogs
    type Rs DescribeMetricFilters = DescribeMetricFiltersResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeMetricFilters where
    next rq rs = (\x -> rq { _dmf1NextToken = Just x })
        <$> (_dmfrsNextToken rs)
