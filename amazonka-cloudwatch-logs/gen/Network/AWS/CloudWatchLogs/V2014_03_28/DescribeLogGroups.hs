{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogGroups where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeLogGroups' request.
describeLogGroups :: DescribeLogGroups
describeLogGroups = DescribeLogGroups
    { _dlgrLimit = Nothing
    , _dlgrLogGroupNamePrefix = Nothing
    , _dlgrNextToken = Nothing
    }

data DescribeLogGroups = DescribeLogGroups
    { _dlgrLimit :: Maybe Integer
      -- ^ The maximum number of items returned in the response. If you
      -- don't specify a value, the request would return up to 50 items.
    , _dlgrLogGroupNamePrefix :: Maybe Text
    , _dlgrNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous DescribeLogGroups request.
    } deriving (Show, Generic)

makeLenses ''DescribeLogGroups

instance ToPath DescribeLogGroups

instance ToQuery DescribeLogGroups

instance ToHeaders DescribeLogGroups

instance ToJSON DescribeLogGroups

data DescribeLogGroupsResponse = DescribeLogGroupsResponse
    { _dlgsLogGroups :: [LogGroup]
      -- ^ A list of log groups.
    , _dlgsNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    } deriving (Show, Generic)

makeLenses ''DescribeLogGroupsResponse

instance FromJSON DescribeLogGroupsResponse

instance AWSRequest DescribeLogGroups where
    type Sv DescribeLogGroups = CloudWatchLogs
    type Rs DescribeLogGroups = DescribeLogGroupsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeLogGroups where
    next rq rs = (\x -> rq { _dlgrNextToken = Just x })
        <$> (_dlgsNextToken rs)
