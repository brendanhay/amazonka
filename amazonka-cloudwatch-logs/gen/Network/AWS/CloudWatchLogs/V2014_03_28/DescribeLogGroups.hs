{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the log groups that are associated with the AWS account making
-- the request. The list returned in the response is ASCII-sorted by log group
-- name. By default, this operation returns up to 50 log groups. If there are
-- more log groups to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log groups returned in
-- the response by specifying the limit parameter in the request. List the log
-- groups for an AWS Account The following is an example of a
-- DescribeLogGroups request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeLogGroups HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]> { "logGroups": [ { "storageBytes": 1048576, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName1:*",
-- "creationTime": 1393545600000, "logGroupName": "exampleLogGroupName1",
-- "metricFilterCount": 0, "retentionInDays": 14 }, { "storageBytes": 5242880,
-- "arn": "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName2:*",
-- "creationTime": 1396224000000, "logGroupName": "exampleLogGroupName2",
-- "metricFilterCount": 0, "retentionInDays": 30 } ] }.
module Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogGroups
    (
    -- * Request
      DescribeLogGroups
    -- ** Request constructor
    , mkDescribeLogGroupsRequest
    -- ** Request lenses
    , dlgsLogGroupNamePrefix
    , dlgsNextToken
    , dlgsLimit

    -- * Response
    , DescribeLogGroupsResponse
    -- ** Response lenses
    , dlgtLogGroups
    , dlgtNextToken
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLogGroups' request.
mkDescribeLogGroupsRequest :: DescribeLogGroups
mkDescribeLogGroupsRequest = DescribeLogGroups
    { _dlgsLogGroupNamePrefix = Nothing
    , _dlgsNextToken = Nothing
    , _dlgsLimit = Nothing
    }
{-# INLINE mkDescribeLogGroupsRequest #-}

data DescribeLogGroups = DescribeLogGroups
    { _dlgsLogGroupNamePrefix :: Maybe Text
    , _dlgsNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous DescribeLogGroups request.
    , _dlgsLimit :: Maybe Integer
      -- ^ The maximum number of items returned in the response. If you
      -- don't specify a value, the request would return up to 50 items.
    } deriving (Show, Generic)

dlgsLogGroupNamePrefix :: Lens' DescribeLogGroups (Maybe Text)
dlgsLogGroupNamePrefix = lens _dlgsLogGroupNamePrefix (\s a -> s { _dlgsLogGroupNamePrefix = a })
{-# INLINE dlgsLogGroupNamePrefix #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous
-- DescribeLogGroups request.
dlgsNextToken :: Lens' DescribeLogGroups (Maybe Text)
dlgsNextToken = lens _dlgsNextToken (\s a -> s { _dlgsNextToken = a })
{-# INLINE dlgsNextToken #-}

-- | The maximum number of items returned in the response. If you don't specify
-- a value, the request would return up to 50 items.
dlgsLimit :: Lens' DescribeLogGroups (Maybe Integer)
dlgsLimit = lens _dlgsLimit (\s a -> s { _dlgsLimit = a })
{-# INLINE dlgsLimit #-}

instance ToPath DescribeLogGroups

instance ToQuery DescribeLogGroups

instance ToHeaders DescribeLogGroups

instance ToJSON DescribeLogGroups

data DescribeLogGroupsResponse = DescribeLogGroupsResponse
    { _dlgtLogGroups :: [LogGroup]
      -- ^ A list of log groups.
    , _dlgtNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    } deriving (Show, Generic)

-- | A list of log groups.
dlgtLogGroups :: Lens' DescribeLogGroupsResponse ([LogGroup])
dlgtLogGroups = lens _dlgtLogGroups (\s a -> s { _dlgtLogGroups = a })
{-# INLINE dlgtLogGroups #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
dlgtNextToken :: Lens' DescribeLogGroupsResponse (Maybe Text)
dlgtNextToken = lens _dlgtNextToken (\s a -> s { _dlgtNextToken = a })
{-# INLINE dlgtNextToken #-}

instance FromJSON DescribeLogGroupsResponse

instance AWSRequest DescribeLogGroups where
    type Sv DescribeLogGroups = CloudWatchLogs
    type Rs DescribeLogGroups = DescribeLogGroupsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeLogGroups where
    next rq rs = (\x -> rq { _dlgsNextToken = Just x })
        <$> (_dlgtNextToken rs)
