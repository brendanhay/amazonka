{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeSnapshots/ action returns information about cache cluster
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
    , dssrsSnapshots
    , dssrsMarker
    , dssrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. dssrsMarker) = Nothing
          | stop (rs ^. dssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dsMarker .~ rs ^. dssrsMarker

instance AWSRequest DescribeSnapshots where
        type Sv DescribeSnapshots = ElastiCache
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = post "DescribeSnapshots"
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
-- * 'dssrsSnapshots'
--
-- * 'dssrsMarker'
--
-- * 'dssrsStatus'
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
    { _dssrsSnapshots :: !(Maybe [Snapshot])
    , _dssrsMarker    :: !(Maybe Text)
    , _dssrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotsResponse' smart constructor.
describeSnapshotsResponse :: Int -> DescribeSnapshotsResponse
describeSnapshotsResponse pStatus_ =
    DescribeSnapshotsResponse'
    { _dssrsSnapshots = Nothing
    , _dssrsMarker = Nothing
    , _dssrsStatus = pStatus_
    }

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
dssrsSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dssrsSnapshots = lens _dssrsSnapshots (\ s a -> s{_dssrsSnapshots = a}) . _Default;

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dssrsMarker :: Lens' DescribeSnapshotsResponse (Maybe Text)
dssrsMarker = lens _dssrsMarker (\ s a -> s{_dssrsMarker = a});

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DescribeSnapshotsResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});
