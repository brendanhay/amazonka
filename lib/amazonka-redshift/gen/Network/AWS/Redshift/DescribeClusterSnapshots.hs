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
-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more snapshot objects, which contain metadata about your cluster snapshots. By default, this operation returns information about all snapshots of all clusters that are owned by you AWS customer account. No information is returned for snapshots owned by inactive AWS customer accounts.
--
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all snapshots that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all snapshots that have any combination of those values are returned. Only snapshots that you own are returned in the response; shared snapshots are not returned with the tag key and tag value request parameters.
--
-- If both tag keys and values are omitted from the request, snapshots are returned regardless of whether they have tag keys or values associated with them.
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSnapshots
    (
    -- * Creating a Request
      describeClusterSnapshots
    , DescribeClusterSnapshots
    -- * Request Lenses
    , dSnapshotIdentifier
    , dTagValues
    , dClusterExists
    , dStartTime
    , dTagKeys
    , dClusterIdentifier
    , dSnapshotType
    , dMarker
    , dMaxRecords
    , dEndTime
    , dOwnerAccount

    -- * Destructuring the Response
    , describeClusterSnapshotsResponse
    , DescribeClusterSnapshotsResponse
    -- * Response Lenses
    , dcssrsSnapshots
    , dcssrsMarker
    , dcssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeClusterSnapshots' smart constructor.
data DescribeClusterSnapshots = DescribeClusterSnapshots'
  { _dSnapshotIdentifier :: !(Maybe Text)
  , _dTagValues          :: !(Maybe [Text])
  , _dClusterExists      :: !(Maybe Bool)
  , _dStartTime          :: !(Maybe ISO8601)
  , _dTagKeys            :: !(Maybe [Text])
  , _dClusterIdentifier  :: !(Maybe Text)
  , _dSnapshotType       :: !(Maybe Text)
  , _dMarker             :: !(Maybe Text)
  , _dMaxRecords         :: !(Maybe Int)
  , _dEndTime            :: !(Maybe ISO8601)
  , _dOwnerAccount       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSnapshotIdentifier' - The snapshot identifier of the snapshot about which to return information.
--
-- * 'dTagValues' - A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
--
-- * 'dClusterExists' - A value that indicates whether to return snapshots only for an existing cluster. Table-level restore can be performed only using a snapshot of an existing cluster, that is, a cluster that has not been deleted. If @ClusterExists@ is set to @true@ , @ClusterIdentifier@ is required.
--
-- * 'dStartTime' - A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: @2012-07-16T18:00:00Z@
--
-- * 'dTagKeys' - A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
--
-- * 'dClusterIdentifier' - The identifier of the cluster for which information about snapshots is requested.
--
-- * 'dSnapshotType' - The type of snapshots for which you are requesting information. By default, snapshots of all types are returned. Valid Values: @automated@ | @manual@
--
-- * 'dMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'dEndTime' - A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: @2012-07-16T18:00:00Z@
--
-- * 'dOwnerAccount' - The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
describeClusterSnapshots
    :: DescribeClusterSnapshots
describeClusterSnapshots =
  DescribeClusterSnapshots'
    { _dSnapshotIdentifier = Nothing
    , _dTagValues = Nothing
    , _dClusterExists = Nothing
    , _dStartTime = Nothing
    , _dTagKeys = Nothing
    , _dClusterIdentifier = Nothing
    , _dSnapshotType = Nothing
    , _dMarker = Nothing
    , _dMaxRecords = Nothing
    , _dEndTime = Nothing
    , _dOwnerAccount = Nothing
    }


-- | The snapshot identifier of the snapshot about which to return information.
dSnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dSnapshotIdentifier = lens _dSnapshotIdentifier (\ s a -> s{_dSnapshotIdentifier = a})

-- | A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
dTagValues :: Lens' DescribeClusterSnapshots [Text]
dTagValues = lens _dTagValues (\ s a -> s{_dTagValues = a}) . _Default . _Coerce

-- | A value that indicates whether to return snapshots only for an existing cluster. Table-level restore can be performed only using a snapshot of an existing cluster, that is, a cluster that has not been deleted. If @ClusterExists@ is set to @true@ , @ClusterIdentifier@ is required.
dClusterExists :: Lens' DescribeClusterSnapshots (Maybe Bool)
dClusterExists = lens _dClusterExists (\ s a -> s{_dClusterExists = a})

-- | A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: @2012-07-16T18:00:00Z@
dStartTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dStartTime = lens _dStartTime (\ s a -> s{_dStartTime = a}) . mapping _Time

-- | A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
dTagKeys :: Lens' DescribeClusterSnapshots [Text]
dTagKeys = lens _dTagKeys (\ s a -> s{_dTagKeys = a}) . _Default . _Coerce

-- | The identifier of the cluster for which information about snapshots is requested.
dClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dClusterIdentifier = lens _dClusterIdentifier (\ s a -> s{_dClusterIdentifier = a})

-- | The type of snapshots for which you are requesting information. By default, snapshots of all types are returned. Valid Values: @automated@ | @manual@
dSnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
dSnapshotType = lens _dSnapshotType (\ s a -> s{_dSnapshotType = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dMarker :: Lens' DescribeClusterSnapshots (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dMaxRecords :: Lens' DescribeClusterSnapshots (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a})

-- | A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Example: @2012-07-16T18:00:00Z@
dEndTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dEndTime = lens _dEndTime (\ s a -> s{_dEndTime = a}) . mapping _Time

-- | The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
dOwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
dOwnerAccount = lens _dOwnerAccount (\ s a -> s{_dOwnerAccount = a})

instance AWSPager DescribeClusterSnapshots where
        page rq rs
          | stop (rs ^. dcssrsMarker) = Nothing
          | stop (rs ^. dcssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dMarker .~ rs ^. dcssrsMarker

instance AWSRequest DescribeClusterSnapshots where
        type Rs DescribeClusterSnapshots =
             DescribeClusterSnapshotsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeClusterSnapshotsResult"
              (\ s h x ->
                 DescribeClusterSnapshotsResponse' <$>
                   (x .@? "Snapshots" .!@ mempty >>=
                      may (parseXMLList "Snapshot"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusterSnapshots where

instance NFData DescribeClusterSnapshots where

instance ToHeaders DescribeClusterSnapshots where
        toHeaders = const mempty

instance ToPath DescribeClusterSnapshots where
        toPath = const "/"

instance ToQuery DescribeClusterSnapshots where
        toQuery DescribeClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterSnapshots" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotIdentifier" =: _dSnapshotIdentifier,
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dTagValues),
               "ClusterExists" =: _dClusterExists,
               "StartTime" =: _dStartTime,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dTagKeys),
               "ClusterIdentifier" =: _dClusterIdentifier,
               "SnapshotType" =: _dSnapshotType,
               "Marker" =: _dMarker, "MaxRecords" =: _dMaxRecords,
               "EndTime" =: _dEndTime,
               "OwnerAccount" =: _dOwnerAccount]

-- | Contains the output from the 'DescribeClusterSnapshots' action.
--
--
--
-- /See:/ 'describeClusterSnapshotsResponse' smart constructor.
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
  { _dcssrsSnapshots      :: !(Maybe [Snapshot])
  , _dcssrsMarker         :: !(Maybe Text)
  , _dcssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcssrsSnapshots' - A list of 'Snapshot' instances.
--
-- * 'dcssrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcssrsResponseStatus' - -- | The response status code.
describeClusterSnapshotsResponse
    :: Int -- ^ 'dcssrsResponseStatus'
    -> DescribeClusterSnapshotsResponse
describeClusterSnapshotsResponse pResponseStatus_ =
  DescribeClusterSnapshotsResponse'
    { _dcssrsSnapshots = Nothing
    , _dcssrsMarker = Nothing
    , _dcssrsResponseStatus = pResponseStatus_
    }


-- | A list of 'Snapshot' instances.
dcssrsSnapshots :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
dcssrsSnapshots = lens _dcssrsSnapshots (\ s a -> s{_dcssrsSnapshots = a}) . _Default . _Coerce

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcssrsMarker :: Lens' DescribeClusterSnapshotsResponse (Maybe Text)
dcssrsMarker = lens _dcssrsMarker (\ s a -> s{_dcssrsMarker = a})

-- | -- | The response status code.
dcssrsResponseStatus :: Lens' DescribeClusterSnapshotsResponse Int
dcssrsResponseStatus = lens _dcssrsResponseStatus (\ s a -> s{_dcssrsResponseStatus = a})

instance NFData DescribeClusterSnapshotsResponse
         where
