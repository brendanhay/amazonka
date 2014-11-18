{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeSnapshots operation returns information about cache cluster
-- snapshots. By default, DescribeSnapshots lists all of your snapshots; it
-- can optionally describe a single snapshot, or just the snapshots associated
-- with a particular cache cluster.
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
    , dsMarker
    , dsMaxRecords
    , dsSnapshotName
    , dsSnapshotSource

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsrMarker
    , dsrSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeSnapshots = DescribeSnapshots
    { _dsCacheClusterId :: Maybe Text
    , _dsMarker         :: Maybe Text
    , _dsMaxRecords     :: Maybe Int
    , _dsSnapshotName   :: Maybe Text
    , _dsSnapshotSource :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeSnapshots' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsCacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'dsMarker' @::@ 'Maybe' 'Text'
--
-- * 'dsMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'dsSnapshotSource' @::@ 'Maybe' 'Text'
--
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots
    { _dsCacheClusterId = Nothing
    , _dsSnapshotName   = Nothing
    , _dsSnapshotSource = Nothing
    , _dsMarker         = Nothing
    , _dsMaxRecords     = Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cache cluster will be described.
dsCacheClusterId :: Lens' DescribeSnapshots (Maybe Text)
dsCacheClusterId = lens _dsCacheClusterId (\s a -> s { _dsCacheClusterId = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dsMarker :: Lens' DescribeSnapshots (Maybe Text)
dsMarker = lens _dsMarker (\s a -> s { _dsMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 50
-- Constraints: minimum 20; maximum 50.
dsMaxRecords :: Lens' DescribeSnapshots (Maybe Int)
dsMaxRecords = lens _dsMaxRecords (\s a -> s { _dsMaxRecords = a })

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot will be described.
dsSnapshotName :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotName = lens _dsSnapshotName (\s a -> s { _dsSnapshotName = a })

-- | If set to system, the output shows snapshots that were automatically
-- created by ElastiCache. If set to user the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
dsSnapshotSource :: Lens' DescribeSnapshots (Maybe Text)
dsSnapshotSource = lens _dsSnapshotSource (\s a -> s { _dsSnapshotSource = a })

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrMarker    :: Maybe Text
    , _dsrSnapshots :: [Snapshot]
    } deriving (Eq, Show, Generic)

-- | 'DescribeSnapshotsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dsrSnapshots' @::@ ['Snapshot']
--
describeSnapshotsResponse :: DescribeSnapshotsResponse
describeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrMarker    = Nothing
    , _dsrSnapshots = mempty
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dsrMarker :: Lens' DescribeSnapshotsResponse (Maybe Text)
dsrMarker = lens _dsrMarker (\s a -> s { _dsrMarker = a })

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
dsrSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsrSnapshots = lens _dsrSnapshots (\s a -> s { _dsrSnapshots = a })

instance ToPath DescribeSnapshots where
    toPath = const "/"

instance ToQuery DescribeSnapshots

instance ToHeaders DescribeSnapshots

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = ElastiCache
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request  = post "DescribeSnapshots"
    response = xmlResponse

instance FromXML DescribeSnapshotsResponse where
    parseXML c = DescribeSnapshotsResponse
        <$> c .:? "Marker"
        <*> c .: "Snapshots"
