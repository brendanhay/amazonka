{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogStreams
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the log streams that are associated with the specified log
-- group. The list returned in the response is ASCII-sorted by log stream
-- name. By default, this operation returns up to 50 log streams. If there are
-- more log streams to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log streams returned in
-- the response by specifying the limit parameter in the request. List the log
-- streams associated with a log group The following is an example of a
-- DescribeLogStreams request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeLogStreams { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "logStreams": [ {
-- "storageBytes": 1048576, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName1:log-stream:exampleLogStreamName1",
-- "creationTime": 1393545600000, "firstEventTimestamp": 1393545600000,
-- "lastEventTimestamp": 1393567800000, "lastIngestionTime": 1393589200000,
-- "logStreamName": "exampleLogStreamName1", "uploadSequenceToken":
-- "88602967394531410094953670125156212707622379445839968487" }, {
-- "storageBytes": 5242880, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName2:log-stream:exampleLogStreamName2",
-- "creationTime": 1396224000000, "firstEventTimestamp": 1396224000000,
-- "lastEventTimestamp": 1396235500000, "lastIngestionTime": 1396225560000,
-- "logStreamName": "exampleLogStreamName2", "uploadSequenceToken":
-- "07622379445839968487886029673945314100949536701251562127" } ] }.
module Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogStreams
    (
    -- * Request
      DescribeLogStreams
    -- ** Request constructor
    , mkDescribeLogStreams
    -- ** Request lenses
    , dls1LogGroupName
    , dls1LogStreamNamePrefix
    , dls1NextToken
    , dls1Limit

    -- * Response
    , DescribeLogStreamsResponse
    -- ** Response lenses
    , dlsrLogStreams
    , dlsrNextToken
    ) where

import Network.AWS.CloudWatchLogs.V2014_03_28.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeLogStreams = DescribeLogStreams
    { _dls1LogGroupName :: Text
    , _dls1LogStreamNamePrefix :: Maybe Text
    , _dls1NextToken :: Maybe Text
    , _dls1Limit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLogStreams' request.
mkDescribeLogStreams :: Text -- ^ 'dls1LogGroupName'
                     -> DescribeLogStreams
mkDescribeLogStreams p1 = DescribeLogStreams
    { _dls1LogGroupName = p1
    , _dls1LogStreamNamePrefix = Nothing
    , _dls1NextToken = Nothing
    , _dls1Limit = Nothing
    }

dls1LogGroupName :: Lens' DescribeLogStreams Text
dls1LogGroupName =
    lens _dls1LogGroupName (\s a -> s { _dls1LogGroupName = a })

dls1LogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dls1LogStreamNamePrefix =
    lens _dls1LogStreamNamePrefix
         (\s a -> s { _dls1LogStreamNamePrefix = a })

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous
-- DescribeLogStreams request.
dls1NextToken :: Lens' DescribeLogStreams (Maybe Text)
dls1NextToken = lens _dls1NextToken (\s a -> s { _dls1NextToken = a })

-- | The maximum number of items returned in the response. If you don't specify
-- a value, the request would return up to 50 items.
dls1Limit :: Lens' DescribeLogStreams (Maybe Integer)
dls1Limit = lens _dls1Limit (\s a -> s { _dls1Limit = a })

instance ToPath DescribeLogStreams

instance ToQuery DescribeLogStreams

instance ToHeaders DescribeLogStreams

instance ToJSON DescribeLogStreams

data DescribeLogStreamsResponse = DescribeLogStreamsResponse
    { _dlsrLogStreams :: [LogStream]
    , _dlsrNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of log streams.
dlsrLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrLogStreams = lens _dlsrLogStreams (\s a -> s { _dlsrLogStreams = a })

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
dlsrNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlsrNextToken = lens _dlsrNextToken (\s a -> s { _dlsrNextToken = a })

instance FromJSON DescribeLogStreamsResponse

instance AWSRequest DescribeLogStreams where
    type Sv DescribeLogStreams = CloudWatchLogs
    type Rs DescribeLogStreams = DescribeLogStreamsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeLogStreams where
    next rq rs = (\x -> rq & dls1NextToken ?~ x)
        <$> (rs ^. dlsrNextToken)
