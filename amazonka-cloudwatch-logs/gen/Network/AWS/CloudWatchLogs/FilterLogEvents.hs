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
-- Module      : Network.AWS.CloudWatchLogs.FilterLogEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log group. You can list all the log events or filter the results using a filter pattern, a time range, and the name of the log stream.
--
--
-- By default, this operation returns as many log events as can fit in 1 MB (up to 10,000 log events), or all the events found within the time range that you specify. If the results include a token, then there are more log events available, and you can get additional results by specifying the token in a subsequent call.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.FilterLogEvents
    (
    -- * Creating a Request
      filterLogEvents
    , FilterLogEvents
    -- * Request Lenses
    , fleStartTime
    , fleNextToken
    , fleLogStreamNames
    , fleEndTime
    , fleLimit
    , fleFilterPattern
    , fleInterleaved
    , fleLogGroupName

    -- * Destructuring the Response
    , filterLogEventsResponse
    , FilterLogEventsResponse
    -- * Response Lenses
    , flersSearchedLogStreams
    , flersNextToken
    , flersEvents
    , flersResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'filterLogEvents' smart constructor.
data FilterLogEvents = FilterLogEvents'
  { _fleStartTime      :: !(Maybe Nat)
  , _fleNextToken      :: !(Maybe Text)
  , _fleLogStreamNames :: !(Maybe (List1 Text))
  , _fleEndTime        :: !(Maybe Nat)
  , _fleLimit          :: !(Maybe Nat)
  , _fleFilterPattern  :: !(Maybe Text)
  , _fleInterleaved    :: !(Maybe Bool)
  , _fleLogGroupName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilterLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleStartTime' - The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp before this time are not returned.
--
-- * 'fleNextToken' - The token for the next set of events to return. (You received this token from a previous call.)
--
-- * 'fleLogStreamNames' - Optional list of log stream names.
--
-- * 'fleEndTime' - The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not returned.
--
-- * 'fleLimit' - The maximum number of events to return. The default is 10,000 events.
--
-- * 'fleFilterPattern' - The filter pattern to use. If not provided, all the events are matched.
--
-- * 'fleInterleaved' - If the value is true, the operation makes a best effort to provide responses that contain events from multiple log streams within the log group, interleaved in a single response. If the value is false, all the matched log events in the first log stream are searched first, then those in the next log stream, and so on. The default is false.
--
-- * 'fleLogGroupName' - The name of the log group.
filterLogEvents
    :: Text -- ^ 'fleLogGroupName'
    -> FilterLogEvents
filterLogEvents pLogGroupName_ =
  FilterLogEvents'
    { _fleStartTime = Nothing
    , _fleNextToken = Nothing
    , _fleLogStreamNames = Nothing
    , _fleEndTime = Nothing
    , _fleLimit = Nothing
    , _fleFilterPattern = Nothing
    , _fleInterleaved = Nothing
    , _fleLogGroupName = pLogGroupName_
    }


-- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp before this time are not returned.
fleStartTime :: Lens' FilterLogEvents (Maybe Natural)
fleStartTime = lens _fleStartTime (\ s a -> s{_fleStartTime = a}) . mapping _Nat

-- | The token for the next set of events to return. (You received this token from a previous call.)
fleNextToken :: Lens' FilterLogEvents (Maybe Text)
fleNextToken = lens _fleNextToken (\ s a -> s{_fleNextToken = a})

-- | Optional list of log stream names.
fleLogStreamNames :: Lens' FilterLogEvents (Maybe (NonEmpty Text))
fleLogStreamNames = lens _fleLogStreamNames (\ s a -> s{_fleLogStreamNames = a}) . mapping _List1

-- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not returned.
fleEndTime :: Lens' FilterLogEvents (Maybe Natural)
fleEndTime = lens _fleEndTime (\ s a -> s{_fleEndTime = a}) . mapping _Nat

-- | The maximum number of events to return. The default is 10,000 events.
fleLimit :: Lens' FilterLogEvents (Maybe Natural)
fleLimit = lens _fleLimit (\ s a -> s{_fleLimit = a}) . mapping _Nat

-- | The filter pattern to use. If not provided, all the events are matched.
fleFilterPattern :: Lens' FilterLogEvents (Maybe Text)
fleFilterPattern = lens _fleFilterPattern (\ s a -> s{_fleFilterPattern = a})

-- | If the value is true, the operation makes a best effort to provide responses that contain events from multiple log streams within the log group, interleaved in a single response. If the value is false, all the matched log events in the first log stream are searched first, then those in the next log stream, and so on. The default is false.
fleInterleaved :: Lens' FilterLogEvents (Maybe Bool)
fleInterleaved = lens _fleInterleaved (\ s a -> s{_fleInterleaved = a})

-- | The name of the log group.
fleLogGroupName :: Lens' FilterLogEvents Text
fleLogGroupName = lens _fleLogGroupName (\ s a -> s{_fleLogGroupName = a})

instance AWSPager FilterLogEvents where
        page rq rs
          | stop (rs ^. flersNextToken) = Nothing
          | otherwise =
            Just $ rq & fleNextToken .~ rs ^. flersNextToken

instance AWSRequest FilterLogEvents where
        type Rs FilterLogEvents = FilterLogEventsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 FilterLogEventsResponse' <$>
                   (x .?> "searchedLogStreams" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (x .?> "events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable FilterLogEvents where

instance NFData FilterLogEvents where

instance ToHeaders FilterLogEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.FilterLogEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON FilterLogEvents where
        toJSON FilterLogEvents'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _fleStartTime,
                  ("nextToken" .=) <$> _fleNextToken,
                  ("logStreamNames" .=) <$> _fleLogStreamNames,
                  ("endTime" .=) <$> _fleEndTime,
                  ("limit" .=) <$> _fleLimit,
                  ("filterPattern" .=) <$> _fleFilterPattern,
                  ("interleaved" .=) <$> _fleInterleaved,
                  Just ("logGroupName" .= _fleLogGroupName)])

instance ToPath FilterLogEvents where
        toPath = const "/"

instance ToQuery FilterLogEvents where
        toQuery = const mempty

-- | /See:/ 'filterLogEventsResponse' smart constructor.
data FilterLogEventsResponse = FilterLogEventsResponse'
  { _flersSearchedLogStreams :: !(Maybe [SearchedLogStream])
  , _flersNextToken          :: !(Maybe Text)
  , _flersEvents             :: !(Maybe [FilteredLogEvent])
  , _flersResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilterLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flersSearchedLogStreams' - Indicates which log streams have been searched and whether each has been searched completely.
--
-- * 'flersNextToken' - The token to use when requesting the next set of items. The token expires after 24 hours.
--
-- * 'flersEvents' - The matched events.
--
-- * 'flersResponseStatus' - -- | The response status code.
filterLogEventsResponse
    :: Int -- ^ 'flersResponseStatus'
    -> FilterLogEventsResponse
filterLogEventsResponse pResponseStatus_ =
  FilterLogEventsResponse'
    { _flersSearchedLogStreams = Nothing
    , _flersNextToken = Nothing
    , _flersEvents = Nothing
    , _flersResponseStatus = pResponseStatus_
    }


-- | Indicates which log streams have been searched and whether each has been searched completely.
flersSearchedLogStreams :: Lens' FilterLogEventsResponse [SearchedLogStream]
flersSearchedLogStreams = lens _flersSearchedLogStreams (\ s a -> s{_flersSearchedLogStreams = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. The token expires after 24 hours.
flersNextToken :: Lens' FilterLogEventsResponse (Maybe Text)
flersNextToken = lens _flersNextToken (\ s a -> s{_flersNextToken = a})

-- | The matched events.
flersEvents :: Lens' FilterLogEventsResponse [FilteredLogEvent]
flersEvents = lens _flersEvents (\ s a -> s{_flersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
flersResponseStatus :: Lens' FilterLogEventsResponse Int
flersResponseStatus = lens _flersResponseStatus (\ s a -> s{_flersResponseStatus = a})

instance NFData FilterLogEventsResponse where
