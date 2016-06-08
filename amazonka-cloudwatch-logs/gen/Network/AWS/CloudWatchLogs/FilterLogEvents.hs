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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves log events, optionally filtered by a filter pattern from the specified log group. You can provide an optional time range to filter the results on the event 'timestamp'. You can limit the streams searched to an explicit list of 'logStreamNames'.
--
-- By default, this operation returns as much matching log events as can fit in a response size of 1MB, up to 10,000 log events, or all the events found within a time-bounded scan window. If the response includes a 'nextToken', then there is more data to search, and the search can be resumed with a new request providing the nextToken. The response will contain a list of 'searchedLogStreams' that contains information about which streams were searched in the request and whether they have been searched completely or require further pagination. The 'limit' parameter in the request. can be used to specify the maximum number of events to return in a page.
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

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilterLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleStartTime'
--
-- * 'fleNextToken'
--
-- * 'fleLogStreamNames'
--
-- * 'fleEndTime'
--
-- * 'fleLimit'
--
-- * 'fleFilterPattern'
--
-- * 'fleInterleaved'
--
-- * 'fleLogGroupName'
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

-- | A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. If provided, events with a timestamp prior to this time are not returned.
fleStartTime :: Lens' FilterLogEvents (Maybe Natural)
fleStartTime = lens _fleStartTime (\ s a -> s{_fleStartTime = a}) . mapping _Nat;

-- | A pagination token obtained from a 'FilterLogEvents' response to continue paginating the FilterLogEvents results. This token is omitted from the response when there are no other events to display.
fleNextToken :: Lens' FilterLogEvents (Maybe Text)
fleNextToken = lens _fleNextToken (\ s a -> s{_fleNextToken = a});

-- | Optional list of log stream names within the specified log group to search. Defaults to all the log streams in the log group.
fleLogStreamNames :: Lens' FilterLogEvents (Maybe (NonEmpty Text))
fleLogStreamNames = lens _fleLogStreamNames (\ s a -> s{_fleLogStreamNames = a}) . mapping _List1;

-- | A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. If provided, events with a timestamp later than this time are not returned.
fleEndTime :: Lens' FilterLogEvents (Maybe Natural)
fleEndTime = lens _fleEndTime (\ s a -> s{_fleEndTime = a}) . mapping _Nat;

-- | The maximum number of events to return in a page of results. Default is 10,000 events.
fleLimit :: Lens' FilterLogEvents (Maybe Natural)
fleLimit = lens _fleLimit (\ s a -> s{_fleLimit = a}) . mapping _Nat;

-- | A valid CloudWatch Logs filter pattern to use for filtering the response. If not provided, all the events are matched.
fleFilterPattern :: Lens' FilterLogEvents (Maybe Text)
fleFilterPattern = lens _fleFilterPattern (\ s a -> s{_fleFilterPattern = a});

-- | If provided, the API will make a best effort to provide responses that contain events from multiple log streams within the log group interleaved in a single response. If not provided, all the matched log events in the first log stream will be searched first, then those in the next log stream, etc.
fleInterleaved :: Lens' FilterLogEvents (Maybe Bool)
fleInterleaved = lens _fleInterleaved (\ s a -> s{_fleInterleaved = a});

-- | The name of the log group to query.
fleLogGroupName :: Lens' FilterLogEvents Text
fleLogGroupName = lens _fleLogGroupName (\ s a -> s{_fleLogGroupName = a});

instance AWSPager FilterLogEvents where
        page rq rs
          | stop (rs ^. flersNextToken) = Nothing
          | stop (rs ^. flersEvents) = Nothing
          | stop (rs ^. flersSearchedLogStreams) = Nothing
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

instance Hashable FilterLogEvents

instance NFData FilterLogEvents

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilterLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flersSearchedLogStreams'
--
-- * 'flersNextToken'
--
-- * 'flersEvents'
--
-- * 'flersResponseStatus'
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

-- | A list of 'SearchedLogStream' objects indicating which log streams have been searched in this request and whether each has been searched completely or still has more to be paginated.
flersSearchedLogStreams :: Lens' FilterLogEventsResponse [SearchedLogStream]
flersSearchedLogStreams = lens _flersSearchedLogStreams (\ s a -> s{_flersSearchedLogStreams = a}) . _Default . _Coerce;

-- | A pagination token obtained from a 'FilterLogEvents' response to continue paginating the FilterLogEvents results. This token is omitted from the response when there are no other events to display.
flersNextToken :: Lens' FilterLogEventsResponse (Maybe Text)
flersNextToken = lens _flersNextToken (\ s a -> s{_flersNextToken = a});

-- | A list of 'FilteredLogEvent' objects representing the matched events from the request.
flersEvents :: Lens' FilterLogEventsResponse [FilteredLogEvent]
flersEvents = lens _flersEvents (\ s a -> s{_flersEvents = a}) . _Default . _Coerce;

-- | The response status code.
flersResponseStatus :: Lens' FilterLogEventsResponse Int
flersResponseStatus = lens _flersResponseStatus (\ s a -> s{_flersResponseStatus = a});

instance NFData FilterLogEventsResponse
