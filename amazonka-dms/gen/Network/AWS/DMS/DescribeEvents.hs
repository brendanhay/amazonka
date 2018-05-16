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
-- Module      : Network.AWS.DMS.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists events for a given source identifier and source type. You can also specify a start and end time. For more information on AWS DMS events, see <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications > .
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEvents
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
    , deersEvents
    , deersMarker
    , deersResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deStartTime        :: !(Maybe POSIX)
  , _deSourceType       :: !(Maybe SourceType)
  , _deFilters          :: !(Maybe [Filter])
  , _deSourceIdentifier :: !(Maybe Text)
  , _deEventCategories  :: !(Maybe [Text])
  , _deMarker           :: !(Maybe Text)
  , _deMaxRecords       :: !(Maybe Int)
  , _deEndTime          :: !(Maybe POSIX)
  , _deDuration         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deStartTime' - The start time for the events to be listed.
--
-- * 'deSourceType' - The type of AWS DMS resource that generates events. Valid values: replication-instance | migration-task
--
-- * 'deFilters' - Filters applied to the action.
--
-- * 'deSourceIdentifier' - The identifier of the event source. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens. It cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'deEventCategories' - A list of event categories for a source type that you want to subscribe to.
--
-- * 'deMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'deMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'deEndTime' - The end time for the events to be listed.
--
-- * 'deDuration' - The duration of the events to be listed.
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


-- | The start time for the events to be listed.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time

-- | The type of AWS DMS resource that generates events. Valid values: replication-instance | migration-task
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\ s a -> s{_deSourceType = a})

-- | Filters applied to the action.
deFilters :: Lens' DescribeEvents [Filter]
deFilters = lens _deFilters (\ s a -> s{_deFilters = a}) . _Default . _Coerce

-- | The identifier of the event source. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens. It cannot end with a hyphen or contain two consecutive hyphens.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier = lens _deSourceIdentifier (\ s a -> s{_deSourceIdentifier = a})

-- | A list of event categories for a source type that you want to subscribe to.
deEventCategories :: Lens' DescribeEvents [Text]
deEventCategories = lens _deEventCategories (\ s a -> s{_deEventCategories = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\ s a -> s{_deMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a})

-- | The end time for the events to be listed.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time

-- | The duration of the events to be listed.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\ s a -> s{_deDuration = a})

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. deersMarker) = Nothing
          | stop (rs ^. deersEvents) = Nothing
          | otherwise =
            Just $ rq & deMarker .~ rs ^. deersMarker

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .?> "Events" .!@ mempty) <*> (x .?> "Marker") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEvents where
        toJSON DescribeEvents'{..}
          = object
              (catMaybes
                 [("StartTime" .=) <$> _deStartTime,
                  ("SourceType" .=) <$> _deSourceType,
                  ("Filters" .=) <$> _deFilters,
                  ("SourceIdentifier" .=) <$> _deSourceIdentifier,
                  ("EventCategories" .=) <$> _deEventCategories,
                  ("Marker" .=) <$> _deMarker,
                  ("MaxRecords" .=) <$> _deMaxRecords,
                  ("EndTime" .=) <$> _deEndTime,
                  ("Duration" .=) <$> _deDuration])

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _deersEvents         :: !(Maybe [Event])
  , _deersMarker         :: !(Maybe Text)
  , _deersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deersEvents' - The events described.
--
-- * 'deersMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'deersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'deersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { _deersEvents = Nothing
    , _deersMarker = Nothing
    , _deersResponseStatus = pResponseStatus_
    }


-- | The events described.
deersEvents :: Lens' DescribeEventsResponse [Event]
deersEvents = lens _deersEvents (\ s a -> s{_deersEvents = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
deersMarker :: Lens' DescribeEventsResponse (Maybe Text)
deersMarker = lens _deersMarker (\ s a -> s{_deersMarker = a})

-- | -- | The response status code.
deersResponseStatus :: Lens' DescribeEventsResponse Int
deersResponseStatus = lens _deersResponseStatus (\ s a -> s{_deersResponseStatus = a})

instance NFData DescribeEventsResponse where
