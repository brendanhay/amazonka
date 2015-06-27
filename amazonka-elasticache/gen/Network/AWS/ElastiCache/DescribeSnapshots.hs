{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
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

-- | The /DescribeSnapshots/ action returns information about cache cluster
-- snapshots. By default, /DescribeSnapshots/ lists all of your snapshots;
-- it can optionally describe a single snapshot, or just the snapshots
-- associated with a particular cache cluster.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeSnapshots.html>
module Network.AWS.ElastiCache.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , dsCacheClusterId
    , dsMaxRecords
    , dsMarker
    , dsSnapshotName
    , dsSnapshotSource

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsr1Snapshots
    , dsr1Marker
    , dsr1Status
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeSnapshotsMessage/ action.
--
-- /See:/ 'describeSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsCacheClusterId'
--
-- * 'dsMaxRecords'
--
-- * 'dsMarker'
--
-- * 'dsSnapshotName'
--
-- * 'dsSnapshotSource'
data DescribeSnapshots = DescribeSnapshots'
    { _dsCacheClusterId :: !(Maybe Text)
    , _dsMaxRecords     :: !(Maybe Int)
    , _dsMarker         :: !(Maybe Text)
    , _dsSnapshotName   :: !(Maybe Text)
    , _dsSnapshotSource :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeSnapshots' smart constructor.
describeSnapshots :: DescribeSnapshots
describeSnapshots =
    DescribeSnapshots'
    { _dsCacheClusterId = Nothing
    , _dsMaxRecords = Nothing
    , _dsMarker = Nothing
    , _dsSnapshotName = Nothing
    , _dsSnapshotSource = Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cache cluster will be described.
dsCacheClusterId :: Lens' DescribeSnapshots (Maybe Text)
dsCacheClusterId = lens _dsCacheClusterId (\ s a -> s{_dsCacheClusterId = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 50
--
-- Constraints: minimum 20; maximum 50.
dsMaxRecords :: Lens' DescribeSnapshots (Maybe Int)
dsMaxRecords = lens _dsMaxRecords (\ s a -> s{_dsMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dsMarker :: Lens' DescribeSnapshots (Maybe Text)
dsMarker = lens _dsMarker (\ s a -> s{_dsMarker = a});

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot will be described.
dsSnapshotName :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotName = lens _dsSnapshotName (\ s a -> s{_dsSnapshotName = a});

-- | If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
dsSnapshotSource :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotSource = lens _dsSnapshotSource (\ s a -> s{_dsSnapshotSource = a});

instance AWSPager DescribeSnapshots where
        page rq rs
          | stop (rs ^. dsr1Marker) = Nothing
          | stop (rs ^. dsr1Snapshots) = Nothing
          | otherwise =
            Just $ rq & dsMarker .~ rs ^. dsr1Marker

instance AWSRequest DescribeSnapshots where
        type Sv DescribeSnapshots = ElastiCache
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeSnapshotsResult"
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .@? "Snapshots" .!@ mempty >>=
                      may (parseXMLList "Snapshot"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _dsMaxRecords, "Marker" =: _dsMarker,
               "SnapshotName" =: _dsSnapshotName,
               "SnapshotSource" =: _dsSnapshotSource]

-- | Represents the output of a /DescribeSnapshots/ action.
--
-- /See:/ 'describeSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsr1Snapshots'
--
-- * 'dsr1Marker'
--
-- * 'dsr1Status'
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
    { _dsr1Snapshots :: !(Maybe [Snapshot])
    , _dsr1Marker    :: !(Maybe Text)
    , _dsr1Status    :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeSnapshotsResponse' smart constructor.
describeSnapshotsResponse :: Int -> DescribeSnapshotsResponse
describeSnapshotsResponse pStatus =
    DescribeSnapshotsResponse'
    { _dsr1Snapshots = Nothing
    , _dsr1Marker = Nothing
    , _dsr1Status = pStatus
    }

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
dsr1Snapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsr1Snapshots = lens _dsr1Snapshots (\ s a -> s{_dsr1Snapshots = a}) . _Default;

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dsr1Marker :: Lens' DescribeSnapshotsResponse (Maybe Text)
dsr1Marker = lens _dsr1Marker (\ s a -> s{_dsr1Marker = a});

-- | FIXME: Undocumented member.
dsr1Status :: Lens' DescribeSnapshotsResponse Int
dsr1Status = lens _dsr1Status (\ s a -> s{_dsr1Status = a});
