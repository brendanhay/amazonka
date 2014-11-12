{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ElastiCache.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshotsMessage
    -- ** Request constructor
    , describeSnapshotsMessage
    -- ** Request lenses
    , dsm1CacheClusterId
    , dsm1Marker
    , dsm1MaxRecords
    , dsm1SnapshotName
    , dsm1SnapshotSource

    -- * Response
    , DescribeSnapshotsListMessage
    -- ** Response constructor
    , describeSnapshotsListMessage
    -- ** Response lenses
    , dslmMarker
    , dslmSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeSnapshotsMessage = DescribeSnapshotsMessage
    { _dsm1CacheClusterId :: Maybe Text
    , _dsm1Marker         :: Maybe Text
    , _dsm1MaxRecords     :: Maybe Int
    , _dsm1SnapshotName   :: Maybe Text
    , _dsm1SnapshotSource :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DescribeSnapshotsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsm1CacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'dsm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dsm1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsm1SnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'dsm1SnapshotSource' @::@ 'Maybe' 'Text'
--
describeSnapshotsMessage :: DescribeSnapshotsMessage
describeSnapshotsMessage = DescribeSnapshotsMessage
    { _dsm1CacheClusterId = Nothing
    , _dsm1SnapshotName   = Nothing
    , _dsm1SnapshotSource = Nothing
    , _dsm1Marker         = Nothing
    , _dsm1MaxRecords     = Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cache cluster will be described.
dsm1CacheClusterId :: Lens' DescribeSnapshotsMessage (Maybe Text)
dsm1CacheClusterId =
    lens _dsm1CacheClusterId (\s a -> s { _dsm1CacheClusterId = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dsm1Marker :: Lens' DescribeSnapshotsMessage (Maybe Text)
dsm1Marker = lens _dsm1Marker (\s a -> s { _dsm1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 50
-- Constraints: minimum 20; maximum 50.
dsm1MaxRecords :: Lens' DescribeSnapshotsMessage (Maybe Int)
dsm1MaxRecords = lens _dsm1MaxRecords (\s a -> s { _dsm1MaxRecords = a })

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot will be described.
dsm1SnapshotName :: Lens' DescribeSnapshotsMessage (Maybe Text)
dsm1SnapshotName = lens _dsm1SnapshotName (\s a -> s { _dsm1SnapshotName = a })

-- | If set to system, the output shows snapshots that were automatically
-- created by ElastiCache. If set to user the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
dsm1SnapshotSource :: Lens' DescribeSnapshotsMessage (Maybe Text)
dsm1SnapshotSource =
    lens _dsm1SnapshotSource (\s a -> s { _dsm1SnapshotSource = a })
instance ToQuery DescribeSnapshotsMessage

instance ToPath DescribeSnapshotsMessage where
    toPath = const "/"

data DescribeSnapshotsListMessage = DescribeSnapshotsListMessage
    { _dslmMarker    :: Maybe Text
    , _dslmSnapshots :: [Snapshot]
    } (Eq, Show, Generic)

-- | 'DescribeSnapshotsListMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dslmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dslmSnapshots' @::@ ['Snapshot']
--
describeSnapshotsListMessage :: DescribeSnapshotsListMessage
describeSnapshotsListMessage = DescribeSnapshotsListMessage
    { _dslmMarker    = Nothing
    , _dslmSnapshots = mempty
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dslmMarker :: Lens' DescribeSnapshotsListMessage (Maybe Text)
dslmMarker = lens _dslmMarker (\s a -> s { _dslmMarker = a })

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
dslmSnapshots :: Lens' DescribeSnapshotsListMessage [Snapshot]
dslmSnapshots = lens _dslmSnapshots (\s a -> s { _dslmSnapshots = a })

instance FromXML DescribeSnapshotsListMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeSnapshotsListMessage"

instance AWSRequest DescribeSnapshotsMessage where
    type Sv DescribeSnapshotsMessage = ElastiCache
    type Rs DescribeSnapshotsMessage = DescribeSnapshotsListMessage

    request  = post "DescribeSnapshots"
    response = xmlResponse $ \h x -> DescribeSnapshotsListMessage
        <$> x %| "Marker"
        <*> x %| "Snapshots"
