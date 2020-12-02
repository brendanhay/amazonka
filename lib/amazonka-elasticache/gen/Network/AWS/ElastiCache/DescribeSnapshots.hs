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
-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster or replication group snapshots. By default, @DescribeSnapshots@ lists all of your snapshots; it can optionally describe a single snapshot, or just the snapshots associated with a particular cache cluster.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeSnapshots
    (
    -- * Creating a Request
      describeSnapshots
    , DescribeSnapshots
    -- * Request Lenses
    , dsCacheClusterId
    , dsMarker
    , dsMaxRecords
    , dsSnapshotName
    , dsShowNodeGroupConfig
    , dsReplicationGroupId
    , dsSnapshotSource

    -- * Destructuring the Response
    , describeSnapshotsResponse
    , DescribeSnapshotsResponse
    -- * Response Lenses
    , dssrsSnapshots
    , dssrsMarker
    , dssrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeSnapshotsMessage@ operation.
--
--
--
-- /See:/ 'describeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { _dsCacheClusterId      :: !(Maybe Text)
  , _dsMarker              :: !(Maybe Text)
  , _dsMaxRecords          :: !(Maybe Int)
  , _dsSnapshotName        :: !(Maybe Text)
  , _dsShowNodeGroupConfig :: !(Maybe Bool)
  , _dsReplicationGroupId  :: !(Maybe Text)
  , _dsSnapshotSource      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsCacheClusterId' - A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
--
-- * 'dsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 50 Constraints: minimum 20; maximum 50.
--
-- * 'dsSnapshotName' - A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
--
-- * 'dsShowNodeGroupConfig' - A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
--
-- * 'dsReplicationGroupId' - A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
--
-- * 'dsSnapshotSource' - If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
describeSnapshots
    :: DescribeSnapshots
describeSnapshots =
  DescribeSnapshots'
    { _dsCacheClusterId = Nothing
    , _dsMarker = Nothing
    , _dsMaxRecords = Nothing
    , _dsSnapshotName = Nothing
    , _dsShowNodeGroupConfig = Nothing
    , _dsReplicationGroupId = Nothing
    , _dsSnapshotSource = Nothing
    }


-- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
dsCacheClusterId :: Lens' DescribeSnapshots (Maybe Text)
dsCacheClusterId = lens _dsCacheClusterId (\ s a -> s{_dsCacheClusterId = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dsMarker :: Lens' DescribeSnapshots (Maybe Text)
dsMarker = lens _dsMarker (\ s a -> s{_dsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 50 Constraints: minimum 20; maximum 50.
dsMaxRecords :: Lens' DescribeSnapshots (Maybe Int)
dsMaxRecords = lens _dsMaxRecords (\ s a -> s{_dsMaxRecords = a})

-- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
dsSnapshotName :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotName = lens _dsSnapshotName (\ s a -> s{_dsSnapshotName = a})

-- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
dsShowNodeGroupConfig :: Lens' DescribeSnapshots (Maybe Bool)
dsShowNodeGroupConfig = lens _dsShowNodeGroupConfig (\ s a -> s{_dsShowNodeGroupConfig = a})

-- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
dsReplicationGroupId :: Lens' DescribeSnapshots (Maybe Text)
dsReplicationGroupId = lens _dsReplicationGroupId (\ s a -> s{_dsReplicationGroupId = a})

-- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
dsSnapshotSource :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotSource = lens _dsSnapshotSource (\ s a -> s{_dsSnapshotSource = a})

instance AWSPager DescribeSnapshots where
        page rq rs
          | stop (rs ^. dssrsMarker) = Nothing
          | stop (rs ^. dssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dsMarker .~ rs ^. dssrsMarker

instance AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DescribeSnapshotsResult"
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .@? "Snapshots" .!@ mempty >>=
                      may (parseXMLList "Snapshot"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSnapshots where

instance NFData DescribeSnapshots where

instance ToHeaders DescribeSnapshots where
        toHeaders = const mempty

instance ToPath DescribeSnapshots where
        toPath = const "/"

instance ToQuery DescribeSnapshots where
        toQuery DescribeSnapshots'{..}
          = mconcat
              ["Action" =: ("DescribeSnapshots" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _dsCacheClusterId,
               "Marker" =: _dsMarker, "MaxRecords" =: _dsMaxRecords,
               "SnapshotName" =: _dsSnapshotName,
               "ShowNodeGroupConfig" =: _dsShowNodeGroupConfig,
               "ReplicationGroupId" =: _dsReplicationGroupId,
               "SnapshotSource" =: _dsSnapshotSource]

-- | Represents the output of a @DescribeSnapshots@ operation.
--
--
--
-- /See:/ 'describeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { _dssrsSnapshots      :: !(Maybe [Snapshot])
  , _dssrsMarker         :: !(Maybe Text)
  , _dssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsSnapshots' - A list of snapshots. Each item in the list contains detailed information about one snapshot.
--
-- * 'dssrsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSnapshotsResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSnapshotsResponse
describeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { _dssrsSnapshots = Nothing
    , _dssrsMarker = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
dssrsSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dssrsSnapshots = lens _dssrsSnapshots (\ s a -> s{_dssrsSnapshots = a}) . _Default . _Coerce

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dssrsMarker :: Lens' DescribeSnapshotsResponse (Maybe Text)
dssrsMarker = lens _dssrsMarker (\ s a -> s{_dssrsMarker = a})

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSnapshotsResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeSnapshotsResponse where
