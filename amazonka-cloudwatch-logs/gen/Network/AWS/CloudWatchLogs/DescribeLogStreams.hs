{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns all the log streams that are associated with the specified log
-- group. The list returned in the response is ASCII-sorted by log stream
-- name.
--
-- By default, this operation returns up to 50 log streams. If there are
-- more log streams to list, the response would contain a @nextToken@ value
-- in the response body. You can also limit the number of log streams
-- returned in the response by specifying the @limit@ parameter in the
-- request. This operation has a limit of five transactions per second,
-- after which transactions are throttled.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html>
module Network.AWS.CloudWatchLogs.DescribeLogStreams
    (
    -- * Request
      DescribeLogStreams
    -- ** Request constructor
    , describeLogStreams
    -- ** Request lenses
    , desOrderBy
    , desDescending
    , desNextToken
    , desLogStreamNamePrefix
    , desLimit
    , desLogGroupName

    -- * Response
    , DescribeLogStreamsResponse
    -- ** Response constructor
    , describeLogStreamsResponse
    -- ** Response lenses
    , dlsrNextToken
    , dlsrLogStreams
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.Types

-- | /See:/ 'describeLogStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desOrderBy'
--
-- * 'desDescending'
--
-- * 'desNextToken'
--
-- * 'desLogStreamNamePrefix'
--
-- * 'desLimit'
--
-- * 'desLogGroupName'
data DescribeLogStreams = DescribeLogStreams'{_desOrderBy :: Maybe OrderBy, _desDescending :: Maybe Bool, _desNextToken :: Maybe Text, _desLogStreamNamePrefix :: Maybe Text, _desLimit :: Maybe Nat, _desLogGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeLogStreams' smart constructor.
describeLogStreams :: Text -> DescribeLogStreams
describeLogStreams pLogGroupName = DescribeLogStreams'{_desOrderBy = Nothing, _desDescending = Nothing, _desNextToken = Nothing, _desLogStreamNamePrefix = Nothing, _desLimit = Nothing, _desLogGroupName = pLogGroupName};

-- | Specifies what to order the returned log streams by. Valid arguments are
-- \'LogStreamName\' or \'LastEventTime\'. If you don\'t specify a value,
-- results are ordered by LogStreamName. If \'LastEventTime\' is chosen,
-- the request cannot also contain a logStreamNamePrefix.
desOrderBy :: Lens' DescribeLogStreams (Maybe OrderBy)
desOrderBy = lens _desOrderBy (\ s a -> s{_desOrderBy = a});

-- | If set to true, results are returned in descending order. If you don\'t
-- specify a value or set it to false, results are returned in ascending
-- order.
desDescending :: Lens' DescribeLogStreams (Maybe Bool)
desDescending = lens _desDescending (\ s a -> s{_desDescending = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeLogStreams@ request.
desNextToken :: Lens' DescribeLogStreams (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | Will only return log streams that match the provided
-- logStreamNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
desLogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
desLogStreamNamePrefix = lens _desLogStreamNamePrefix (\ s a -> s{_desLogStreamNamePrefix = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
desLimit :: Lens' DescribeLogStreams (Maybe Natural)
desLimit = lens _desLimit (\ s a -> s{_desLimit = a}) . mapping _Nat;

-- | The log group name for which log streams are to be listed.
desLogGroupName :: Lens' DescribeLogStreams Text
desLogGroupName = lens _desLogGroupName (\ s a -> s{_desLogGroupName = a});

instance AWSRequest DescribeLogStreams where
        type Sv DescribeLogStreams = CloudWatchLogs
        type Rs DescribeLogStreams =
             DescribeLogStreamsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLogStreamsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "logStreams" .!@ mempty))

instance ToHeaders DescribeLogStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeLogStreams" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLogStreams where
        toJSON DescribeLogStreams'{..}
          = object
              ["orderBy" .= _desOrderBy,
               "descending" .= _desDescending,
               "nextToken" .= _desNextToken,
               "logStreamNamePrefix" .= _desLogStreamNamePrefix,
               "limit" .= _desLimit,
               "logGroupName" .= _desLogGroupName]

instance ToPath DescribeLogStreams where
        toPath = const "/"

instance ToQuery DescribeLogStreams where
        toQuery = const mempty

-- | /See:/ 'describeLogStreamsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsrNextToken'
--
-- * 'dlsrLogStreams'
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'{_dlsrNextToken :: Maybe Text, _dlsrLogStreams :: Maybe [LogStream]} deriving (Eq, Read, Show)

-- | 'DescribeLogStreamsResponse' smart constructor.
describeLogStreamsResponse :: DescribeLogStreamsResponse
describeLogStreamsResponse = DescribeLogStreamsResponse'{_dlsrNextToken = Nothing, _dlsrLogStreams = Nothing};

-- | FIXME: Undocumented member.
dlsrNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlsrNextToken = lens _dlsrNextToken (\ s a -> s{_dlsrNextToken = a});

-- | FIXME: Undocumented member.
dlsrLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrLogStreams = lens _dlsrLogStreams (\ s a -> s{_dlsrLogStreams = a}) . _Default;
