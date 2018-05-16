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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.
--
--
-- This operation has a limit of five transactions per second, after which transactions are throttled.
--
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

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
  { _dlssOrderBy             :: !(Maybe OrderBy)
  , _dlssDescending          :: !(Maybe Bool)
  , _dlssNextToken           :: !(Maybe Text)
  , _dlssLogStreamNamePrefix :: !(Maybe Text)
  , _dlssLimit               :: !(Maybe Nat)
  , _dlssLogGroupName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLogStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlssOrderBy' - If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ . If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter. lastEventTimestamp represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. lastEventTimeStamp updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but may take longer in some rare situations.
--
-- * 'dlssDescending' - If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
--
-- * 'dlssNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dlssLogStreamNamePrefix' - The prefix to match. iIf @orderBy@ is @LastEventTime@ ,you cannot specify this parameter.
--
-- * 'dlssLimit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- * 'dlssLogGroupName' - The name of the log group.
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


-- | If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ . If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter. lastEventTimestamp represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. lastEventTimeStamp updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but may take longer in some rare situations.
dlssOrderBy :: Lens' DescribeLogStreams (Maybe OrderBy)
dlssOrderBy = lens _dlssOrderBy (\ s a -> s{_dlssOrderBy = a})

-- | If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
dlssDescending :: Lens' DescribeLogStreams (Maybe Bool)
dlssDescending = lens _dlssDescending (\ s a -> s{_dlssDescending = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
dlssNextToken :: Lens' DescribeLogStreams (Maybe Text)
dlssNextToken = lens _dlssNextToken (\ s a -> s{_dlssNextToken = a})

-- | The prefix to match. iIf @orderBy@ is @LastEventTime@ ,you cannot specify this parameter.
dlssLogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dlssLogStreamNamePrefix = lens _dlssLogStreamNamePrefix (\ s a -> s{_dlssLogStreamNamePrefix = a})

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
dlssLimit :: Lens' DescribeLogStreams (Maybe Natural)
dlssLimit = lens _dlssLimit (\ s a -> s{_dlssLimit = a}) . mapping _Nat

-- | The name of the log group.
dlssLogGroupName :: Lens' DescribeLogStreams Text
dlssLogGroupName = lens _dlssLogGroupName (\ s a -> s{_dlssLogGroupName = a})

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

instance Hashable DescribeLogStreams where

instance NFData DescribeLogStreams where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLogStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsNextToken' - Undocumented member.
--
-- * 'dlsrsLogStreams' - The log streams.
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
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
dlsrsNextToken = lens _dlsrsNextToken (\ s a -> s{_dlsrsNextToken = a})

-- | The log streams.
dlsrsLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrsLogStreams = lens _dlsrsLogStreams (\ s a -> s{_dlsrsLogStreams = a}) . _Default . _Coerce

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DescribeLogStreamsResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\ s a -> s{_dlsrsResponseStatus = a})

instance NFData DescribeLogStreamsResponse where
