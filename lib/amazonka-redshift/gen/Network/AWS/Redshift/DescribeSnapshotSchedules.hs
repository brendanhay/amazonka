{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeSnapshotSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot schedules.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotSchedules
  ( -- * Creating a Request
    describeSnapshotSchedules,
    DescribeSnapshotSchedules,

    -- * Request Lenses
    dssTagValues,
    dssTagKeys,
    dssClusterIdentifier,
    dssMarker,
    dssMaxRecords,
    dssScheduleIdentifier,

    -- * Destructuring the Response
    describeSnapshotSchedulesResponse,
    DescribeSnapshotSchedulesResponse,

    -- * Response Lenses
    dssrsSnapshotSchedules,
    dssrsMarker,
    dssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { _dssTagValues ::
      !(Maybe [Text]),
    _dssTagKeys :: !(Maybe [Text]),
    _dssClusterIdentifier :: !(Maybe Text),
    _dssMarker :: !(Maybe Text),
    _dssMaxRecords :: !(Maybe Int),
    _dssScheduleIdentifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSnapshotSchedules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssTagValues' - The value corresponding to the key of the snapshot schedule tag.
--
-- * 'dssTagKeys' - The key value for a snapshot schedule tag.
--
-- * 'dssClusterIdentifier' - The unique identifier for the cluster whose snapshot schedules you want to view.
--
-- * 'dssMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dssMaxRecords' - The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
--
-- * 'dssScheduleIdentifier' - A unique identifier for a snapshot schedule.
describeSnapshotSchedules ::
  DescribeSnapshotSchedules
describeSnapshotSchedules =
  DescribeSnapshotSchedules'
    { _dssTagValues = Nothing,
      _dssTagKeys = Nothing,
      _dssClusterIdentifier = Nothing,
      _dssMarker = Nothing,
      _dssMaxRecords = Nothing,
      _dssScheduleIdentifier = Nothing
    }

-- | The value corresponding to the key of the snapshot schedule tag.
dssTagValues :: Lens' DescribeSnapshotSchedules [Text]
dssTagValues = lens _dssTagValues (\s a -> s {_dssTagValues = a}) . _Default . _Coerce

-- | The key value for a snapshot schedule tag.
dssTagKeys :: Lens' DescribeSnapshotSchedules [Text]
dssTagKeys = lens _dssTagKeys (\s a -> s {_dssTagKeys = a}) . _Default . _Coerce

-- | The unique identifier for the cluster whose snapshot schedules you want to view.
dssClusterIdentifier :: Lens' DescribeSnapshotSchedules (Maybe Text)
dssClusterIdentifier = lens _dssClusterIdentifier (\s a -> s {_dssClusterIdentifier = a})

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
dssMarker :: Lens' DescribeSnapshotSchedules (Maybe Text)
dssMarker = lens _dssMarker (\s a -> s {_dssMarker = a})

-- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
dssMaxRecords :: Lens' DescribeSnapshotSchedules (Maybe Int)
dssMaxRecords = lens _dssMaxRecords (\s a -> s {_dssMaxRecords = a})

-- | A unique identifier for a snapshot schedule.
dssScheduleIdentifier :: Lens' DescribeSnapshotSchedules (Maybe Text)
dssScheduleIdentifier = lens _dssScheduleIdentifier (\s a -> s {_dssScheduleIdentifier = a})

instance AWSPager DescribeSnapshotSchedules where
  page rq rs
    | stop (rs ^. dssrsMarker) = Nothing
    | stop (rs ^. dssrsSnapshotSchedules) = Nothing
    | otherwise = Just $ rq & dssMarker .~ rs ^. dssrsMarker

instance AWSRequest DescribeSnapshotSchedules where
  type
    Rs DescribeSnapshotSchedules =
      DescribeSnapshotSchedulesResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "DescribeSnapshotSchedulesResult"
      ( \s h x ->
          DescribeSnapshotSchedulesResponse'
            <$> ( x .@? "SnapshotSchedules" .!@ mempty
                    >>= may (parseXMLList "SnapshotSchedule")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSnapshotSchedules

instance NFData DescribeSnapshotSchedules

instance ToHeaders DescribeSnapshotSchedules where
  toHeaders = const mempty

instance ToPath DescribeSnapshotSchedules where
  toPath = const "/"

instance ToQuery DescribeSnapshotSchedules where
  toQuery DescribeSnapshotSchedules' {..} =
    mconcat
      [ "Action" =: ("DescribeSnapshotSchedules" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "TagValues" =: toQuery (toQueryList "TagValue" <$> _dssTagValues),
        "TagKeys" =: toQuery (toQueryList "TagKey" <$> _dssTagKeys),
        "ClusterIdentifier" =: _dssClusterIdentifier,
        "Marker" =: _dssMarker,
        "MaxRecords" =: _dssMaxRecords,
        "ScheduleIdentifier" =: _dssScheduleIdentifier
      ]

-- | /See:/ 'describeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { _dssrsSnapshotSchedules ::
      !( Maybe
           [SnapshotSchedule]
       ),
    _dssrsMarker ::
      !(Maybe Text),
    _dssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSnapshotSchedulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsSnapshotSchedules' - A list of SnapshotSchedules.
--
-- * 'dssrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSnapshotSchedulesResponse ::
  -- | 'dssrsResponseStatus'
  Int ->
  DescribeSnapshotSchedulesResponse
describeSnapshotSchedulesResponse pResponseStatus_ =
  DescribeSnapshotSchedulesResponse'
    { _dssrsSnapshotSchedules =
        Nothing,
      _dssrsMarker = Nothing,
      _dssrsResponseStatus = pResponseStatus_
    }

-- | A list of SnapshotSchedules.
dssrsSnapshotSchedules :: Lens' DescribeSnapshotSchedulesResponse [SnapshotSchedule]
dssrsSnapshotSchedules = lens _dssrsSnapshotSchedules (\s a -> s {_dssrsSnapshotSchedules = a}) . _Default . _Coerce

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
dssrsMarker :: Lens' DescribeSnapshotSchedulesResponse (Maybe Text)
dssrsMarker = lens _dssrsMarker (\s a -> s {_dssrsMarker = a})

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSnapshotSchedulesResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\s a -> s {_dssrsResponseStatus = a})

instance NFData DescribeSnapshotSchedulesResponse
