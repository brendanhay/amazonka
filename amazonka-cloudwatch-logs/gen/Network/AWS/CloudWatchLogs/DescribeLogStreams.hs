{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the log streams that are associated with the specified log
-- group. The list returned in the response is ASCII-sorted by log stream
-- name.
--
-- By default, this operation returns up to 50 log streams. If there are
-- more log streams to list, the response would contain a 'nextToken' value
-- in the response body. You can also limit the number of log streams
-- returned in the response by specifying the 'limit' parameter in the
-- request. This operation has a limit of five transactions per second,
-- after which transactions are throttled.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogStreams
    (
    -- * Creating a Request
      describeLogStreams
    , DescribeLogStreams
    -- * Request Lenses
    , dlssOrderBy
    , dlssDescending
    , dlssNextToken
    , dlssLogStreamNamePrefix
    , dlssLimit
    , dlssLogGroupName

    -- * Destructuring the Response
    , describeLogStreamsResponse
    , DescribeLogStreamsResponse
    -- * Response Lenses
    , dlsrsNextToken
    , dlsrsLogStreams
    , dlsrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
    { _dlssOrderBy             :: !(Maybe OrderBy)
    , _dlssDescending          :: !(Maybe Bool)
    , _dlssNextToken           :: !(Maybe Text)
    , _dlssLogStreamNamePrefix :: !(Maybe Text)
    , _dlssLimit               :: !(Maybe Nat)
    , _dlssLogGroupName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLogStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeLogStreams
    :: Text -- ^ 'dlssLogGroupName'
    -> DescribeLogStreams
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
-- 'DescribeLogStreams' request.
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

instance AWSPager DescribeLogStreams where
        page rq rs
          | stop (rs ^. dlsrsNextToken) = Nothing
          | stop (rs ^. dlsrsLogStreams) = Nothing
          | otherwise =
            Just $ rq & dlssNextToken .~ rs ^. dlsrsNextToken

instance AWSRequest DescribeLogStreams where
        type Rs DescribeLogStreams =
             DescribeLogStreamsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLogStreamsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "logStreams" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLogStreams

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
              (catMaybes
                 [("orderBy" .=) <$> _dlssOrderBy,
                  ("descending" .=) <$> _dlssDescending,
                  ("nextToken" .=) <$> _dlssNextToken,
                  ("logStreamNamePrefix" .=) <$>
                    _dlssLogStreamNamePrefix,
                  ("limit" .=) <$> _dlssLimit,
                  Just ("logGroupName" .= _dlssLogGroupName)])

instance ToPath DescribeLogStreams where
        toPath = const "/"

instance ToQuery DescribeLogStreams where
        toQuery = const mempty

-- | /See:/ 'describeLogStreamsResponse' smart constructor.
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
    { _dlsrsNextToken      :: !(Maybe Text)
    , _dlsrsLogStreams     :: !(Maybe [LogStream])
    , _dlsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLogStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsNextToken'
--
-- * 'dlsrsLogStreams'
--
-- * 'dlsrsResponseStatus'
describeLogStreamsResponse
    :: Int -- ^ 'dlsrsResponseStatus'
    -> DescribeLogStreamsResponse
describeLogStreamsResponse pResponseStatus_ =
    DescribeLogStreamsResponse'
    { _dlsrsNextToken = Nothing
    , _dlsrsLogStreams = Nothing
    , _dlsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dlsrsNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlsrsNextToken = lens _dlsrsNextToken (\ s a -> s{_dlsrsNextToken = a});

-- | Undocumented member.
dlsrsLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrsLogStreams = lens _dlsrsLogStreams (\ s a -> s{_dlsrsLogStreams = a}) . _Default . _Coerce;

-- | The response status code.
dlsrsResponseStatus :: Lens' DescribeLogStreamsResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\ s a -> s{_dlsrsResponseStatus = a});
