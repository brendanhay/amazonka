{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all the log streams that are associated with the specified log
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
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html AWS API Reference> for DescribeLogStreams.
module Network.AWS.CloudWatchLogs.DescribeLogStreams
    (
    -- * Creating a Request
      DescribeLogStreams
    , describeLogStreams
    -- * Request Lenses
    , dlssOrderBy
    , dlssDescending
    , dlssNextToken
    , dlssLogStreamNamePrefix
    , dlssLimit
    , dlssLogGroupName

    -- * Destructuring the Response
    , DescribeLogStreamsResponse
    , describeLogStreamsResponse
    -- * Response Lenses
    , dlsrsNextToken
    , dlsrsLogStreams
    , dlsrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLogStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlssOrderBy'
--
-- * 'dlssDescending'
--
-- * 'dlssNextToken'
--
-- * 'dlssLogStreamNamePrefix'
--
-- * 'dlssLimit'
--
-- * 'dlssLogGroupName'
data DescribeLogStreams = DescribeLogStreams'
    { _dlssOrderBy             :: !(Maybe OrderBy)
    , _dlssDescending          :: !(Maybe Bool)
    , _dlssNextToken           :: !(Maybe Text)
    , _dlssLogStreamNamePrefix :: !(Maybe Text)
    , _dlssLimit               :: !(Maybe Nat)
    , _dlssLogGroupName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogStreams' smart constructor.
describeLogStreams :: Text -> DescribeLogStreams
describeLogStreams pLogGroupName_ =
    DescribeLogStreams'
    { _dlssOrderBy = Nothing
    , _dlssDescending = Nothing
    , _dlssNextToken = Nothing
    , _dlssLogStreamNamePrefix = Nothing
    , _dlssLimit = Nothing
    , _dlssLogGroupName = pLogGroupName_
    }

-- | Specifies what to order the returned log streams by. Valid arguments are
-- \'LogStreamName\' or \'LastEventTime\'. If you don\'t specify a value,
-- results are ordered by LogStreamName. If \'LastEventTime\' is chosen,
-- the request cannot also contain a logStreamNamePrefix.
dlssOrderBy :: Lens' DescribeLogStreams (Maybe OrderBy)
dlssOrderBy = lens _dlssOrderBy (\ s a -> s{_dlssOrderBy = a});

-- | If set to true, results are returned in descending order. If you don\'t
-- specify a value or set it to false, results are returned in ascending
-- order.
dlssDescending :: Lens' DescribeLogStreams (Maybe Bool)
dlssDescending = lens _dlssDescending (\ s a -> s{_dlssDescending = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeLogStreams@ request.
dlssNextToken :: Lens' DescribeLogStreams (Maybe Text)
dlssNextToken = lens _dlssNextToken (\ s a -> s{_dlssNextToken = a});

-- | Will only return log streams that match the provided
-- logStreamNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dlssLogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dlssLogStreamNamePrefix = lens _dlssLogStreamNamePrefix (\ s a -> s{_dlssLogStreamNamePrefix = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dlssLimit :: Lens' DescribeLogStreams (Maybe Natural)
dlssLimit = lens _dlssLimit (\ s a -> s{_dlssLimit = a}) . mapping _Nat;

-- | The log group name for which log streams are to be listed.
dlssLogGroupName :: Lens' DescribeLogStreams Text
dlssLogGroupName = lens _dlssLogGroupName (\ s a -> s{_dlssLogGroupName = a});

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
                     (x .?> "logStreams" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["orderBy" .= _dlssOrderBy,
               "descending" .= _dlssDescending,
               "nextToken" .= _dlssNextToken,
               "logStreamNamePrefix" .= _dlssLogStreamNamePrefix,
               "limit" .= _dlssLimit,
               "logGroupName" .= _dlssLogGroupName]

instance ToPath DescribeLogStreams where
        toPath = const "/"

instance ToQuery DescribeLogStreams where
        toQuery = const mempty

-- | /See:/ 'describeLogStreamsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsrsNextToken'
--
-- * 'dlsrsLogStreams'
--
-- * 'dlsrsStatus'
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
    { _dlsrsNextToken  :: !(Maybe Text)
    , _dlsrsLogStreams :: !(Maybe [LogStream])
    , _dlsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogStreamsResponse' smart constructor.
describeLogStreamsResponse :: Int -> DescribeLogStreamsResponse
describeLogStreamsResponse pStatus_ =
    DescribeLogStreamsResponse'
    { _dlsrsNextToken = Nothing
    , _dlsrsLogStreams = Nothing
    , _dlsrsStatus = pStatus_
    }

-- | Undocumented member.
dlsrsNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlsrsNextToken = lens _dlsrsNextToken (\ s a -> s{_dlsrsNextToken = a});

-- | Undocumented member.
dlsrsLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrsLogStreams = lens _dlsrsLogStreams (\ s a -> s{_dlsrsLogStreams = a}) . _Default . _Coerce;

-- | Undocumented member.
dlsrsStatus :: Lens' DescribeLogStreamsResponse Int
dlsrsStatus = lens _dlsrsStatus (\ s a -> s{_dlsrsStatus = a});
