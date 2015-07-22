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
    , dsrqCacheClusterId
    , dsrqMaxRecords
    , dsrqMarker
    , dsrqSnapshotName
    , dsrqSnapshotSource

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
-- * 'dsrqCacheClusterId'
--
-- * 'dsrqMaxRecords'
--
-- * 'dsrqMarker'
--
-- * 'dsrqSnapshotName'
--
-- * 'dsrqSnapshotSource'
data DescribeSnapshots = DescribeSnapshots'
    { _dsrqCacheClusterId :: !(Maybe Text)
    , _dsrqMaxRecords     :: !(Maybe Int)
    , _dsrqMarker         :: !(Maybe Text)
    , _dsrqSnapshotName   :: !(Maybe Text)
    , _dsrqSnapshotSource :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshots' smart constructor.
describeSnapshots :: DescribeSnapshots
describeSnapshots =
    DescribeSnapshots'
    { _dsrqCacheClusterId = Nothing
    , _dsrqMaxRecords = Nothing
    , _dsrqMarker = Nothing
    , _dsrqSnapshotName = Nothing
    , _dsrqSnapshotSource = Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cache cluster will be described.
dsrqCacheClusterId :: Lens' DescribeSnapshots (Maybe Text)
dsrqCacheClusterId = lens _dsrqCacheClusterId (\ s a -> s{_dsrqCacheClusterId = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 50
--
-- Constraints: minimum 20; maximum 50.
dsrqMaxRecords :: Lens' DescribeSnapshots (Maybe Int)
dsrqMaxRecords = lens _dsrqMaxRecords (\ s a -> s{_dsrqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dsrqMarker :: Lens' DescribeSnapshots (Maybe Text)
dsrqMarker = lens _dsrqMarker (\ s a -> s{_dsrqMarker = a});

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot will be described.
dsrqSnapshotName :: Lens' DescribeSnapshots (Maybe Text)
dsrqSnapshotName = lens _dsrqSnapshotName (\ s a -> s{_dsrqSnapshotName = a});

-- | If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
dsrqSnapshotSource :: Lens' DescribeSnapshots (Maybe Text)
dsrqSnapshotSource = lens _dsrqSnapshotSource (\ s a -> s{_dsrqSnapshotSource = a});

instance AWSPager DescribeSnapshots where
        page rq rs
          | stop (rs ^. dssrsMarker) = Nothing
          | stop (rs ^. dssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dsrqMarker .~ rs ^. dssrsMarker

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
               "CacheClusterId" =: _dsrqCacheClusterId,
               "MaxRecords" =: _dsrqMaxRecords,
               "Marker" =: _dsrqMarker,
               "SnapshotName" =: _dsrqSnapshotName,
               "SnapshotSource" =: _dsrqSnapshotSource]

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
describeSnapshotsResponse pStatus =
    DescribeSnapshotsResponse'
    { _dssrsSnapshots = Nothing
    , _dssrsMarker = Nothing
    , _dssrsStatus = pStatus
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
