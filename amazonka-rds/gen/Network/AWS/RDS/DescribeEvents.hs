{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DB instances, DB security groups, DB
-- snapshots, and DB parameter groups for the past 14 days. Events specific
-- to a particular DB instance, DB security group, database snapshot, or DB
-- parameter group can be obtained by providing the name as a parameter. By
-- default, the past hour of events are returned.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEvents.html>
module Network.AWS.RDS.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , derqStartTime
    , derqSourceType
    , derqFilters
    , derqSourceIdentifier
    , derqMaxRecords
    , derqEventCategories
    , derqEndTime
    , derqMarker
    , derqDuration

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , dersEvents
    , dersMarker
    , dersStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derqStartTime'
--
-- * 'derqSourceType'
--
-- * 'derqFilters'
--
-- * 'derqSourceIdentifier'
--
-- * 'derqMaxRecords'
--
-- * 'derqEventCategories'
--
-- * 'derqEndTime'
--
-- * 'derqMarker'
--
-- * 'derqDuration'
data DescribeEvents = DescribeEvents'
    { _derqStartTime        :: !(Maybe ISO8601)
    , _derqSourceType       :: !(Maybe SourceType)
    , _derqFilters          :: !(Maybe [Filter])
    , _derqSourceIdentifier :: !(Maybe Text)
    , _derqMaxRecords       :: !(Maybe Int)
    , _derqEventCategories  :: !(Maybe [Text])
    , _derqEndTime          :: !(Maybe ISO8601)
    , _derqMarker           :: !(Maybe Text)
    , _derqDuration         :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEvents' smart constructor.
describeEvents :: DescribeEvents
describeEvents =
    DescribeEvents'
    { _derqStartTime = Nothing
    , _derqSourceType = Nothing
    , _derqFilters = Nothing
    , _derqSourceIdentifier = Nothing
    , _derqMaxRecords = Nothing
    , _derqEventCategories = Nothing
    , _derqEndTime = Nothing
    , _derqMarker = Nothing
    , _derqDuration = Nothing
    }

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
derqStartTime :: Lens' DescribeEvents (Maybe UTCTime)
derqStartTime = lens _derqStartTime (\ s a -> s{_derqStartTime = a}) . mapping _Time;

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
derqSourceType :: Lens' DescribeEvents (Maybe SourceType)
derqSourceType = lens _derqSourceType (\ s a -> s{_derqSourceType = a});

-- | This parameter is not currently supported.
derqFilters :: Lens' DescribeEvents [Filter]
derqFilters = lens _derqFilters (\ s a -> s{_derqFilters = a}) . _Default;

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
--
-- Constraints:
--
-- -   If SourceIdentifier is supplied, SourceType must also be provided.
-- -   If the source type is @DBInstance@, then a @DBInstanceIdentifier@
--     must be supplied.
-- -   If the source type is @DBSecurityGroup@, a @DBSecurityGroupName@
--     must be supplied.
-- -   If the source type is @DBParameterGroup@, a @DBParameterGroupName@
--     must be supplied.
-- -   If the source type is @DBSnapshot@, a @DBSnapshotIdentifier@ must be
--     supplied.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
derqSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
derqSourceIdentifier = lens _derqSourceIdentifier (\ s a -> s{_derqSourceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
derqMaxRecords :: Lens' DescribeEvents (Maybe Int)
derqMaxRecords = lens _derqMaxRecords (\ s a -> s{_derqMaxRecords = a});

-- | A list of event categories that trigger notifications for a event
-- notification subscription.
derqEventCategories :: Lens' DescribeEvents [Text]
derqEventCategories = lens _derqEventCategories (\ s a -> s{_derqEventCategories = a}) . _Default;

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
derqEndTime :: Lens' DescribeEvents (Maybe UTCTime)
derqEndTime = lens _derqEndTime (\ s a -> s{_derqEndTime = a}) . mapping _Time;

-- | An optional pagination token provided by a previous DescribeEvents
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
derqMarker :: Lens' DescribeEvents (Maybe Text)
derqMarker = lens _derqMarker (\ s a -> s{_derqMarker = a});

-- | The number of minutes to retrieve events for.
--
-- Default: 60
derqDuration :: Lens' DescribeEvents (Maybe Int)
derqDuration = lens _derqDuration (\ s a -> s{_derqDuration = a});

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersMarker) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & derqMarker .~ rs ^. dersMarker

instance AWSRequest DescribeEvents where
        type Sv DescribeEvents = RDS
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
               "Version" =: ("2014-10-31" :: ByteString),
               "StartTime" =: _derqStartTime,
               "SourceType" =: _derqSourceType,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _derqFilters),
               "SourceIdentifier" =: _derqSourceIdentifier,
               "MaxRecords" =: _derqMaxRecords,
               "EventCategories" =:
                 toQuery
                   (toQueryList "EventCategory" <$>
                      _derqEventCategories),
               "EndTime" =: _derqEndTime, "Marker" =: _derqMarker,
               "Duration" =: _derqDuration]

-- | Contains the result of a successful invocation of the DescribeEvents
-- action.
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
describeEventsResponse pStatus =
    DescribeEventsResponse'
    { _dersEvents = Nothing
    , _dersMarker = Nothing
    , _dersStatus = pStatus
    }

-- | A list of Event instances.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default;

-- | An optional pagination token provided by a previous Events request. If
-- this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by @MaxRecords@ .
dersMarker :: Lens' DescribeEventsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a});

-- | FIXME: Undocumented member.
dersStatus :: Lens' DescribeEventsResponse Int
dersStatus = lens _dersStatus (\ s a -> s{_dersStatus = a});
