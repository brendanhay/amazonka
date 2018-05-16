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
-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DB instances, DB security groups, DB snapshots, and DB parameter groups for the past 14 days. Events specific to a particular DB instance, DB security group, database snapshot, or DB parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deStartTime
    , deSourceType
    , deFilters
    , deSourceIdentifier
    , deEventCategories
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

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deStartTime        :: !(Maybe ISO8601)
  , _deSourceType       :: !(Maybe SourceType)
  , _deFilters          :: !(Maybe [Filter])
  , _deSourceIdentifier :: !(Maybe Text)
  , _deEventCategories  :: !(Maybe [Text])
  , _deMarker           :: !(Maybe Text)
  , _deMaxRecords       :: !(Maybe Int)
  , _deEndTime          :: !(Maybe ISO8601)
  , _deDuration         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deStartTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: 2009-07-08T18:00Z
--
-- * 'deSourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
--
-- * 'deFilters' - This parameter is not currently supported.
--
-- * 'deSourceIdentifier' - The identifier of the event source for which events are returned. If not specified, then all sources are included in the response. Constraints:     * If SourceIdentifier is supplied, SourceType must also be provided.     * If the source type is @DBInstance@ , then a @DBInstanceIdentifier@ must be supplied.     * If the source type is @DBSecurityGroup@ , a @DBSecurityGroupName@ must be supplied.     * If the source type is @DBParameterGroup@ , a @DBParameterGroupName@ must be supplied.     * If the source type is @DBSnapshot@ , a @DBSnapshotIdentifier@ must be supplied.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'deEventCategories' - A list of event categories that trigger notifications for a event notification subscription.
--
-- * 'deMarker' - An optional pagination token provided by a previous DescribeEvents request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'deMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'deEndTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: 2009-07-08T18:00Z
--
-- * 'deDuration' - The number of minutes to retrieve events for. Default: 60
describeEvents
    :: DescribeEvents
describeEvents =
  DescribeEvents'
    { _deStartTime = Nothing
    , _deSourceType = Nothing
    , _deFilters = Nothing
    , _deSourceIdentifier = Nothing
    , _deEventCategories = Nothing
    , _deMarker = Nothing
    , _deMaxRecords = Nothing
    , _deEndTime = Nothing
    , _deDuration = Nothing
    }


-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: 2009-07-08T18:00Z
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time

-- | The event source to retrieve events for. If no value is specified, all events are returned.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\ s a -> s{_deSourceType = a})

-- | This parameter is not currently supported.
deFilters :: Lens' DescribeEvents [Filter]
deFilters = lens _deFilters (\ s a -> s{_deFilters = a}) . _Default . _Coerce

-- | The identifier of the event source for which events are returned. If not specified, then all sources are included in the response. Constraints:     * If SourceIdentifier is supplied, SourceType must also be provided.     * If the source type is @DBInstance@ , then a @DBInstanceIdentifier@ must be supplied.     * If the source type is @DBSecurityGroup@ , a @DBSecurityGroupName@ must be supplied.     * If the source type is @DBParameterGroup@ , a @DBParameterGroupName@ must be supplied.     * If the source type is @DBSnapshot@ , a @DBSnapshotIdentifier@ must be supplied.     * Cannot end with a hyphen or contain two consecutive hyphens.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier = lens _deSourceIdentifier (\ s a -> s{_deSourceIdentifier = a})

-- | A list of event categories that trigger notifications for a event notification subscription.
deEventCategories :: Lens' DescribeEvents [Text]
deEventCategories = lens _deEventCategories (\ s a -> s{_deEventCategories = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous DescribeEvents request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\ s a -> s{_deMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a})

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: 2009-07-08T18:00Z
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time

-- | The number of minutes to retrieve events for. Default: 60
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
        request = postQuery rds
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
               "Version" =: ("2014-10-31" :: ByteString),
               "StartTime" =: _deStartTime,
               "SourceType" =: _deSourceType,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _deFilters),
               "SourceIdentifier" =: _deSourceIdentifier,
               "EventCategories" =:
                 toQuery
                   (toQueryList "EventCategory" <$> _deEventCategories),
               "Marker" =: _deMarker, "MaxRecords" =: _deMaxRecords,
               "EndTime" =: _deEndTime, "Duration" =: _deDuration]

-- | Contains the result of a successful invocation of the 'DescribeEvents' action.
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
-- * 'dersEvents' - A list of 'Event' instances.
--
-- * 'dersMarker' - An optional pagination token provided by a previous Events request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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


-- | A list of 'Event' instances.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous Events request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dersMarker :: Lens' DescribeEventsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEventsResponse where
