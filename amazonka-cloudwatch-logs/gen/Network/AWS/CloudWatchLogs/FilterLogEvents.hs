{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.FilterLogEvents
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

-- | Retrieves log events, optionally filtered by a filter pattern from the
-- specified log group. You can provide an optional time range to filter
-- the results on the event @timestamp@. You can limit the streams searched
-- to an explicit list of @logStreamNames@.
--
-- By default, this operation returns as much matching log events as can
-- fit in a response size of 1MB, up to 10,000 log events, or all the
-- events found within a time-bounded scan window. If the response includes
-- a @nextToken@, then there is more data to search, and the search can be
-- resumed with a new request providing the nextToken. The response will
-- contain a list of @searchedLogStreams@ that contains information about
-- which streams were searched in the request and whether they have been
-- searched completely or require further pagination. The @limit@ parameter
-- in the request. can be used to specify the maximum number of events to
-- return in a page.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_FilterLogEvents.html>
module Network.AWS.CloudWatchLogs.FilterLogEvents
    (
    -- * Request
      FilterLogEvents
    -- ** Request constructor
    , filterLogEvents
    -- ** Request lenses
    , fleStartTime
    , fleEndTime
    , fleFilterPattern
    , fleInterleaved
    , fleLogGroupName
    , fleLogStreamNames
    , fleNextToken
    , fleLimit

    -- * Response
    , FilterLogEventsResponse
    -- ** Response constructor
    , filterLogEventsResponse
    -- ** Response lenses
    , flerSearchedLogStreams
    , flerEvents
    , flerNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.Types

-- | /See:/ 'filterLogEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fleStartTime'
--
-- * 'fleEndTime'
--
-- * 'fleFilterPattern'
--
-- * 'fleInterleaved'
--
-- * 'fleLogGroupName'
--
-- * 'fleLogStreamNames'
--
-- * 'fleNextToken'
--
-- * 'fleLimit'
data FilterLogEvents = FilterLogEvents'{_fleStartTime :: Maybe Nat, _fleEndTime :: Maybe Nat, _fleFilterPattern :: Maybe Text, _fleInterleaved :: Maybe Bool, _fleLogGroupName :: Text, _fleLogStreamNames :: List1 Text, _fleNextToken :: Text, _fleLimit :: Nat} deriving (Eq, Read, Show)

-- | 'FilterLogEvents' smart constructor.
filterLogEvents :: Text -> NonEmpty Text -> Text -> Natural -> FilterLogEvents
filterLogEvents pLogGroupName pLogStreamNames pNextToken pLimit = FilterLogEvents'{_fleStartTime = Nothing, _fleEndTime = Nothing, _fleFilterPattern = Nothing, _fleInterleaved = Nothing, _fleLogGroupName = pLogGroupName, _fleLogStreamNames = _List1 # pLogStreamNames, _fleNextToken = pNextToken, _fleLimit = _Nat # pLimit};

-- | A unix timestamp indicating the start time of the range for the request.
-- If provided, events with a timestamp prior to this time will not be
-- returned.
fleStartTime :: Lens' FilterLogEvents (Maybe Natural)
fleStartTime = lens _fleStartTime (\ s a -> s{_fleStartTime = a}) . mapping _Nat;

-- | A unix timestamp indicating the end time of the range for the request.
-- If provided, events with a timestamp later than this time will not be
-- returned.
fleEndTime :: Lens' FilterLogEvents (Maybe Natural)
fleEndTime = lens _fleEndTime (\ s a -> s{_fleEndTime = a}) . mapping _Nat;

-- | A valid CloudWatch Logs filter pattern to use for filtering the
-- response. If not provided, all the events are matched.
fleFilterPattern :: Lens' FilterLogEvents (Maybe Text)
fleFilterPattern = lens _fleFilterPattern (\ s a -> s{_fleFilterPattern = a});

-- | If provided, the API will make a best effort to provide responses that
-- contain events from multiple log streams within the log group
-- interleaved in a single response. If not provided, all the matched log
-- events in the first log stream will be searched first, then those in the
-- next log stream, etc.
fleInterleaved :: Lens' FilterLogEvents (Maybe Bool)
fleInterleaved = lens _fleInterleaved (\ s a -> s{_fleInterleaved = a});

-- | The name of the log group to query.
fleLogGroupName :: Lens' FilterLogEvents Text
fleLogGroupName = lens _fleLogGroupName (\ s a -> s{_fleLogGroupName = a});

-- | Optional list of log stream names within the specified log group to
-- search. Defaults to all the log streams in the log group.
fleLogStreamNames :: Lens' FilterLogEvents (NonEmpty Text)
fleLogStreamNames = lens _fleLogStreamNames (\ s a -> s{_fleLogStreamNames = a}) . _List1;

-- | A pagination token obtained from a @FilterLogEvents@ response to
-- continue paginating the FilterLogEvents results.
fleNextToken :: Lens' FilterLogEvents Text
fleNextToken = lens _fleNextToken (\ s a -> s{_fleNextToken = a});

-- | The maximum number of events to return in a page of results. Default is
-- 10,000 events.
fleLimit :: Lens' FilterLogEvents Natural
fleLimit = lens _fleLimit (\ s a -> s{_fleLimit = a}) . _Nat;

instance AWSRequest FilterLogEvents where
        type Sv FilterLogEvents = CloudWatchLogs
        type Rs FilterLogEvents = FilterLogEventsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 FilterLogEventsResponse' <$>
                   x .?> "searchedLogStreams" .!@ mempty <*>
                     x .?> "events" .!@ mempty
                     <*> x .:> "nextToken")

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
              ["startTime" .= _fleStartTime,
               "endTime" .= _fleEndTime,
               "filterPattern" .= _fleFilterPattern,
               "interleaved" .= _fleInterleaved,
               "logGroupName" .= _fleLogGroupName,
               "logStreamNames" .= _fleLogStreamNames,
               "nextToken" .= _fleNextToken, "limit" .= _fleLimit]

instance ToPath FilterLogEvents where
        toPath = const "/"

instance ToQuery FilterLogEvents where
        toQuery = const mempty

-- | /See:/ 'filterLogEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'flerSearchedLogStreams'
--
-- * 'flerEvents'
--
-- * 'flerNextToken'
data FilterLogEventsResponse = FilterLogEventsResponse'{_flerSearchedLogStreams :: [SearchedLogStream], _flerEvents :: [FilteredLogEvent], _flerNextToken :: Text} deriving (Eq, Read, Show)

-- | 'FilterLogEventsResponse' smart constructor.
filterLogEventsResponse :: Text -> FilterLogEventsResponse
filterLogEventsResponse pNextToken = FilterLogEventsResponse'{_flerSearchedLogStreams = mempty, _flerEvents = mempty, _flerNextToken = pNextToken};

-- | A list of @SearchedLogStream@ objects indicating which log streams have
-- been searched in this request and whether each has been searched
-- completely or still has more to be paginated.
flerSearchedLogStreams :: Lens' FilterLogEventsResponse [SearchedLogStream]
flerSearchedLogStreams = lens _flerSearchedLogStreams (\ s a -> s{_flerSearchedLogStreams = a});

-- | A list of @FilteredLogEvent@ objects representing the matched events
-- from the request.
flerEvents :: Lens' FilterLogEventsResponse [FilteredLogEvent]
flerEvents = lens _flerEvents (\ s a -> s{_flerEvents = a});

-- | A pagination token obtained from a @FilterLogEvents@ response to
-- continue paginating the FilterLogEvents results.
flerNextToken :: Lens' FilterLogEventsResponse Text
flerNextToken = lens _flerNextToken (\ s a -> s{_flerNextToken = a});
