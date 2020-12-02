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
-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, cache security groups, and cache parameter groups. You can obtain events specific to a particular cluster, cache security group, or cache parameter group by providing the name as a parameter.
--
--
-- By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deStartTime
    , deSourceType
    , deSourceIdentifier
    , deMarker
    , deMaxRecords
    , deEndTime
    , deDuration

    -- * Destructuring the Response
    , describeEventsResponse
    , DescribeEventsResponse
    -- * Response Lenses
    , dersEvents
    , dersMarker
    , dersResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeEvents@ operation.
--
--
--
-- /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deStartTime        :: !(Maybe ISO8601)
  , _deSourceType       :: !(Maybe SourceType)
  , _deSourceIdentifier :: !(Maybe Text)
  , _deMarker           :: !(Maybe Text)
  , _deMaxRecords       :: !(Maybe Int)
  , _deEndTime          :: !(Maybe ISO8601)
  , _deDuration         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deStartTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format. __Example:__ 2017-03-30T07:03:49.555Z
--
-- * 'deSourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
--
-- * 'deSourceIdentifier' - The identifier of the event source for which events are returned. If not specified, all sources are included in the response.
--
-- * 'deMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'deMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'deEndTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format. __Example:__ 2017-03-30T07:03:49.555Z
--
-- * 'deDuration' - The number of minutes worth of events to retrieve.
describeEvents
    :: DescribeEvents
describeEvents =
  DescribeEvents'
    { _deStartTime = Nothing
    , _deSourceType = Nothing
    , _deSourceIdentifier = Nothing
    , _deMarker = Nothing
    , _deMaxRecords = Nothing
    , _deEndTime = Nothing
    , _deDuration = Nothing
    }


-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format. __Example:__ 2017-03-30T07:03:49.555Z
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time

-- | The event source to retrieve events for. If no value is specified, all events are returned.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\ s a -> s{_deSourceType = a})

-- | The identifier of the event source for which events are returned. If not specified, all sources are included in the response.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier = lens _deSourceIdentifier (\ s a -> s{_deSourceIdentifier = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\ s a -> s{_deMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a})

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format. __Example:__ 2017-03-30T07:03:49.555Z
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time

-- | The number of minutes worth of events to retrieve.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\ s a -> s{_deDuration = a})

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersMarker) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & deMarker .~ rs ^. dersMarker

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DescribeEventsResult"
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .@? "Events" .!@ mempty >>=
                      may (parseXMLList "Event"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders = const mempty

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery DescribeEvents'{..}
          = mconcat
              ["Action" =: ("DescribeEvents" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "StartTime" =: _deStartTime,
               "SourceType" =: _deSourceType,
               "SourceIdentifier" =: _deSourceIdentifier,
               "Marker" =: _deMarker, "MaxRecords" =: _deMaxRecords,
               "EndTime" =: _deEndTime, "Duration" =: _deDuration]

-- | Represents the output of a @DescribeEvents@ operation.
--
--
--
-- /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _dersEvents         :: !(Maybe [Event])
  , _dersMarker         :: !(Maybe Text)
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersEvents' - A list of events. Each element in the list contains detailed information about one event.
--
-- * 'dersMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { _dersEvents = Nothing
    , _dersMarker = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | A list of events. Each element in the list contains detailed information about one event.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dersMarker :: Lens' DescribeEventsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEventsResponse where
