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
    , mkDescribeLogStreamsRequest
    -- ** Request lenses
    , dlssLogGroupName
    , dlssLogStreamNamePrefix
    , dlssNextToken
    , dlssLimit

    -- * Response
    , DescribeLogStreamsResponse
    -- ** Response lenses
    , dlstLogStreams
    , dlstNextToken
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLogStreams' request.
mkDescribeLogStreamsRequest :: Text -- ^ 'dlssLogGroupName'
                            -> DescribeLogStreams
mkDescribeLogStreamsRequest p1 = DescribeLogStreams
    { _dlssLogGroupName = p1
    , _dlssLogStreamNamePrefix = Nothing
    , _dlssNextToken = Nothing
    , _dlssLimit = Nothing
    }
{-# INLINE mkDescribeLogStreamsRequest #-}

data DescribeLogStreams = DescribeLogStreams
    { _dlssLogGroupName :: Text
    , _dlssLogStreamNamePrefix :: Maybe Text
    , _dlssNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous DescribeLogStreams request.
    , _dlssLimit :: Maybe Integer
      -- ^ The maximum number of items returned in the response. If you
      -- don't specify a value, the request would return up to 50 items.
    } deriving (Show, Generic)

dlssLogGroupName :: Lens' DescribeLogStreams (Text)
dlssLogGroupName = lens _dlssLogGroupName (\s a -> s { _dlssLogGroupName = a })
{-# INLINE dlssLogGroupName #-}

dlssLogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dlssLogStreamNamePrefix = lens _dlssLogStreamNamePrefix (\s a -> s { _dlssLogStreamNamePrefix = a })
{-# INLINE dlssLogStreamNamePrefix #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous
-- DescribeLogStreams request.
dlssNextToken :: Lens' DescribeLogStreams (Maybe Text)
dlssNextToken = lens _dlssNextToken (\s a -> s { _dlssNextToken = a })
{-# INLINE dlssNextToken #-}

-- | The maximum number of items returned in the response. If you don't specify
-- a value, the request would return up to 50 items.
dlssLimit :: Lens' DescribeLogStreams (Maybe Integer)
dlssLimit = lens _dlssLimit (\s a -> s { _dlssLimit = a })
{-# INLINE dlssLimit #-}

instance ToPath DescribeLogStreams

instance ToQuery DescribeLogStreams

instance ToHeaders DescribeLogStreams

instance ToJSON DescribeLogStreams

data DescribeLogStreamsResponse = DescribeLogStreamsResponse
    { _dlstLogStreams :: [LogStream]
      -- ^ A list of log streams.
    , _dlstNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    } deriving (Show, Generic)

-- | A list of log streams.
dlstLogStreams :: Lens' DescribeLogStreamsResponse ([LogStream])
dlstLogStreams = lens _dlstLogStreams (\s a -> s { _dlstLogStreams = a })
{-# INLINE dlstLogStreams #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
dlstNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlstNextToken = lens _dlstNextToken (\s a -> s { _dlstNextToken = a })
{-# INLINE dlstNextToken #-}

instance FromJSON DescribeLogStreamsResponse

instance AWSRequest DescribeLogStreams where
    type Sv DescribeLogStreams = CloudWatchLogs
    type Rs DescribeLogStreams = DescribeLogStreamsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeLogStreams where
    next rq rs = (\x -> rq { _dlssNextToken = Just x })
        <$> (_dlstNextToken rs)
