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
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html>
module Network.AWS.CloudWatchLogs.DescribeLogStreams
    (
    -- * Request
      DescribeLogStreams
    -- ** Request constructor
    , describeLogStreams
    -- ** Request lenses
    , dlssrqOrderBy
    , dlssrqDescending
    , dlssrqNextToken
    , dlssrqLogStreamNamePrefix
    , dlssrqLimit
    , dlssrqLogGroupName

    -- * Response
    , DescribeLogStreamsResponse
    -- ** Response constructor
    , describeLogStreamsResponse
    -- ** Response lenses
    , dlssrsNextToken
    , dlssrsLogStreams
    , dlssrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLogStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlssrqOrderBy'
--
-- * 'dlssrqDescending'
--
-- * 'dlssrqNextToken'
--
-- * 'dlssrqLogStreamNamePrefix'
--
-- * 'dlssrqLimit'
--
-- * 'dlssrqLogGroupName'
data DescribeLogStreams = DescribeLogStreams'
    { _dlssrqOrderBy             :: !(Maybe OrderBy)
    , _dlssrqDescending          :: !(Maybe Bool)
    , _dlssrqNextToken           :: !(Maybe Text)
    , _dlssrqLogStreamNamePrefix :: !(Maybe Text)
    , _dlssrqLimit               :: !(Maybe Nat)
    , _dlssrqLogGroupName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogStreams' smart constructor.
describeLogStreams :: Text -> DescribeLogStreams
describeLogStreams pLogGroupName =
    DescribeLogStreams'
    { _dlssrqOrderBy = Nothing
    , _dlssrqDescending = Nothing
    , _dlssrqNextToken = Nothing
    , _dlssrqLogStreamNamePrefix = Nothing
    , _dlssrqLimit = Nothing
    , _dlssrqLogGroupName = pLogGroupName
    }

-- | Specifies what to order the returned log streams by. Valid arguments are
-- \'LogStreamName\' or \'LastEventTime\'. If you don\'t specify a value,
-- results are ordered by LogStreamName. If \'LastEventTime\' is chosen,
-- the request cannot also contain a logStreamNamePrefix.
dlssrqOrderBy :: Lens' DescribeLogStreams (Maybe OrderBy)
dlssrqOrderBy = lens _dlssrqOrderBy (\ s a -> s{_dlssrqOrderBy = a});

-- | If set to true, results are returned in descending order. If you don\'t
-- specify a value or set it to false, results are returned in ascending
-- order.
dlssrqDescending :: Lens' DescribeLogStreams (Maybe Bool)
dlssrqDescending = lens _dlssrqDescending (\ s a -> s{_dlssrqDescending = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeLogStreams@ request.
dlssrqNextToken :: Lens' DescribeLogStreams (Maybe Text)
dlssrqNextToken = lens _dlssrqNextToken (\ s a -> s{_dlssrqNextToken = a});

-- | Will only return log streams that match the provided
-- logStreamNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dlssrqLogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dlssrqLogStreamNamePrefix = lens _dlssrqLogStreamNamePrefix (\ s a -> s{_dlssrqLogStreamNamePrefix = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dlssrqLimit :: Lens' DescribeLogStreams (Maybe Natural)
dlssrqLimit = lens _dlssrqLimit (\ s a -> s{_dlssrqLimit = a}) . mapping _Nat;

-- | The log group name for which log streams are to be listed.
dlssrqLogGroupName :: Lens' DescribeLogStreams Text
dlssrqLogGroupName = lens _dlssrqLogGroupName (\ s a -> s{_dlssrqLogGroupName = a});

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
              ["orderBy" .= _dlssrqOrderBy,
               "descending" .= _dlssrqDescending,
               "nextToken" .= _dlssrqNextToken,
               "logStreamNamePrefix" .= _dlssrqLogStreamNamePrefix,
               "limit" .= _dlssrqLimit,
               "logGroupName" .= _dlssrqLogGroupName]

instance ToPath DescribeLogStreams where
        toPath = const "/"

instance ToQuery DescribeLogStreams where
        toQuery = const mempty

-- | /See:/ 'describeLogStreamsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlssrsNextToken'
--
-- * 'dlssrsLogStreams'
--
-- * 'dlssrsStatus'
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
    { _dlssrsNextToken  :: !(Maybe Text)
    , _dlssrsLogStreams :: !(Maybe [LogStream])
    , _dlssrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogStreamsResponse' smart constructor.
describeLogStreamsResponse :: Int -> DescribeLogStreamsResponse
describeLogStreamsResponse pStatus =
    DescribeLogStreamsResponse'
    { _dlssrsNextToken = Nothing
    , _dlssrsLogStreams = Nothing
    , _dlssrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dlssrsNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlssrsNextToken = lens _dlssrsNextToken (\ s a -> s{_dlssrsNextToken = a});

-- | FIXME: Undocumented member.
dlssrsLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlssrsLogStreams = lens _dlssrsLogStreams (\ s a -> s{_dlssrsLogStreams = a}) . _Default;

-- | FIXME: Undocumented member.
dlssrsStatus :: Lens' DescribeLogStreamsResponse Int
dlssrsStatus = lens _dlssrsStatus (\ s a -> s{_dlssrsStatus = a});
