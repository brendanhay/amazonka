{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeEvents/ action returns events related to cache clusters,
-- cache security groups, and cache parameter groups. You can obtain events
-- specific to a particular cache cluster, cache security group, or cache
-- parameter group by providing the name as a parameter.
--
-- By default, only the events occurring within the last hour are returned;
-- however, you can retrieve up to 14 days\' worth of events if necessary.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html>
module Network.AWS.ElastiCache.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , deStartTime
    , deSourceType
    , deSourceIdentifier
    , deMaxRecords
    , deEndTime
    , deMarker
    , deDuration

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , dersEvents
    , dersMarker
    , dersStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeEvents/ action.
--
-- /See:/ 'describeEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deStartTime'
--
-- * 'deSourceType'
--
-- * 'deSourceIdentifier'
--
-- * 'deMaxRecords'
--
-- * 'deEndTime'
--
-- * 'deMarker'
--
-- * 'deDuration'
data DescribeEvents = DescribeEvents'
    { _deStartTime        :: !(Maybe ISO8601)
    , _deSourceType       :: !(Maybe SourceType)
    , _deSourceIdentifier :: !(Maybe Text)
    , _deMaxRecords       :: !(Maybe Int)
    , _deEndTime          :: !(Maybe ISO8601)
    , _deMarker           :: !(Maybe Text)
    , _deDuration         :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEvents' smart constructor.
describeEvents :: DescribeEvents
describeEvents =
    DescribeEvents'
    { _deStartTime = Nothing
    , _deSourceType = Nothing
    , _deSourceIdentifier = Nothing
    , _deMaxRecords = Nothing
    , _deEndTime = Nothing
    , _deMarker = Nothing
    , _deDuration = Nothing
    }

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time;

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
--
-- Valid values are: @cache-cluster@ | @cache-parameter-group@ |
-- @cache-security-group@ | @cache-subnet-group@
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\ s a -> s{_deSourceType = a});

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier = lens _deSourceIdentifier (\ s a -> s{_deSourceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a});

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time;

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\ s a -> s{_deMarker = a});

-- | The number of minutes\' worth of events to retrieve.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\ s a -> s{_deDuration = a});

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersMarker) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & deMarker .~ rs ^. dersMarker

instance AWSRequest DescribeEvents where
        type Sv DescribeEvents = ElastiCache
        type Rs DescribeEvents = DescribeEventsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeEventsResult"
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .@? "Events" .!@ mempty >>=
                      may (parseXMLList "Event"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _deMaxRecords,
               "EndTime" =: _deEndTime, "Marker" =: _deMarker,
               "Duration" =: _deDuration]

-- | Represents the output of a /DescribeEvents/ action.
--
-- /See:/ 'describeEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dersEvents'
--
-- * 'dersMarker'
--
-- * 'dersStatus'
data DescribeEventsResponse = DescribeEventsResponse'
    { _dersEvents :: !(Maybe [Event])
    , _dersMarker :: !(Maybe Text)
    , _dersStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventsResponse' smart constructor.
describeEventsResponse :: Int -> DescribeEventsResponse
describeEventsResponse pStatus_ =
    DescribeEventsResponse'
    { _dersEvents = Nothing
    , _dersMarker = Nothing
    , _dersStatus = pStatus_
    }

-- | A list of events. Each element in the list contains detailed information
-- about one event.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dersMarker :: Lens' DescribeEventsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a});

-- | FIXME: Undocumented member.
dersStatus :: Lens' DescribeEventsResponse Int
dersStatus = lens _dersStatus (\ s a -> s{_dersStatus = a});
