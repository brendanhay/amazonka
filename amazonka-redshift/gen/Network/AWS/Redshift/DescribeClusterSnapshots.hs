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

-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about all
-- snapshots of all clusters that are owned by you AWS customer account. No
-- information is returned for snapshots owned by inactive AWS customer
-- accounts.
module Network.AWS.Redshift.DescribeClusterSnapshots
    (
    -- * Request
      DescribeClusterSnapshotsMessage
    -- ** Request constructor
    , describeClusterSnapshotsMessage
    -- ** Request lenses
    , dcsm1ClusterIdentifier
    , dcsm1EndTime
    , dcsm1Marker
    , dcsm1MaxRecords
    , dcsm1OwnerAccount
    , dcsm1SnapshotIdentifier
    , dcsm1SnapshotType
    , dcsm1StartTime

    -- * Response
    , SnapshotMessage
    -- ** Response constructor
    , snapshotMessage
    -- ** Response lenses
    , smMarker
    , smSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterSnapshotsMessage = DescribeClusterSnapshotsMessage
    { _dcsm1ClusterIdentifier  :: Maybe Text
    , _dcsm1EndTime            :: Maybe RFC822
    , _dcsm1Marker             :: Maybe Text
    , _dcsm1MaxRecords         :: Maybe Int
    , _dcsm1OwnerAccount       :: Maybe Text
    , _dcsm1SnapshotIdentifier :: Maybe Text
    , _dcsm1SnapshotType       :: Maybe Text
    , _dcsm1StartTime          :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterSnapshotsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsm1ClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcsm1EndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dcsm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsm1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcsm1OwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'dcsm1SnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcsm1SnapshotType' @::@ 'Maybe' 'Text'
--
-- * 'dcsm1StartTime' @::@ 'Maybe' 'UTCTime'
--
describeClusterSnapshotsMessage :: DescribeClusterSnapshotsMessage
describeClusterSnapshotsMessage = DescribeClusterSnapshotsMessage
    { _dcsm1ClusterIdentifier  = Nothing
    , _dcsm1SnapshotIdentifier = Nothing
    , _dcsm1SnapshotType       = Nothing
    , _dcsm1StartTime          = Nothing
    , _dcsm1EndTime            = Nothing
    , _dcsm1MaxRecords         = Nothing
    , _dcsm1Marker             = Nothing
    , _dcsm1OwnerAccount       = Nothing
    }

-- | The identifier of the cluster for which information about snapshots is
-- requested.
dcsm1ClusterIdentifier :: Lens' DescribeClusterSnapshotsMessage (Maybe Text)
dcsm1ClusterIdentifier =
    lens _dcsm1ClusterIdentifier (\s a -> s { _dcsm1ClusterIdentifier = a })

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the ISO8601 Wikipedia page. Example:
-- 2012-07-16T18:00:00Z.
dcsm1EndTime :: Lens' DescribeClusterSnapshotsMessage (Maybe UTCTime)
dcsm1EndTime = lens _dcsm1EndTime (\s a -> s { _dcsm1EndTime = a })
    . mapping _Time

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcsm1Marker :: Lens' DescribeClusterSnapshotsMessage (Maybe Text)
dcsm1Marker = lens _dcsm1Marker (\s a -> s { _dcsm1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsm1MaxRecords :: Lens' DescribeClusterSnapshotsMessage (Maybe Int)
dcsm1MaxRecords = lens _dcsm1MaxRecords (\s a -> s { _dcsm1MaxRecords = a })

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
dcsm1OwnerAccount :: Lens' DescribeClusterSnapshotsMessage (Maybe Text)
dcsm1OwnerAccount =
    lens _dcsm1OwnerAccount (\s a -> s { _dcsm1OwnerAccount = a })

-- | The snapshot identifier of the snapshot about which to return
-- information.
dcsm1SnapshotIdentifier :: Lens' DescribeClusterSnapshotsMessage (Maybe Text)
dcsm1SnapshotIdentifier =
    lens _dcsm1SnapshotIdentifier (\s a -> s { _dcsm1SnapshotIdentifier = a })

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned. Valid Values: automated |
-- manual.
dcsm1SnapshotType :: Lens' DescribeClusterSnapshotsMessage (Maybe Text)
dcsm1SnapshotType =
    lens _dcsm1SnapshotType (\s a -> s { _dcsm1SnapshotType = a })

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the ISO8601 Wikipedia page. Example:
-- 2012-07-16T18:00:00Z.
dcsm1StartTime :: Lens' DescribeClusterSnapshotsMessage (Maybe UTCTime)
dcsm1StartTime = lens _dcsm1StartTime (\s a -> s { _dcsm1StartTime = a })
    . mapping _Time

instance ToPath DescribeClusterSnapshotsMessage where
    toPath = const "/"

instance ToQuery DescribeClusterSnapshotsMessage

data SnapshotMessage = SnapshotMessage
    { _smMarker    :: Maybe Text
    , _smSnapshots :: [Snapshot]
    } deriving (Eq, Show, Generic)

-- | 'SnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smMarker' @::@ 'Maybe' 'Text'
--
-- * 'smSnapshots' @::@ ['Snapshot']
--
snapshotMessage :: SnapshotMessage
snapshotMessage = SnapshotMessage
    { _smMarker    = Nothing
    , _smSnapshots = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
smMarker :: Lens' SnapshotMessage (Maybe Text)
smMarker = lens _smMarker (\s a -> s { _smMarker = a })

-- | A list of Snapshot instances.
smSnapshots :: Lens' SnapshotMessage [Snapshot]
smSnapshots = lens _smSnapshots (\s a -> s { _smSnapshots = a })

instance AWSRequest DescribeClusterSnapshotsMessage where
    type Sv DescribeClusterSnapshotsMessage = Redshift
    type Rs DescribeClusterSnapshotsMessage = SnapshotMessage

    request  = post "DescribeClusterSnapshots"
    response = const . xmlResponse $ \h x -> SnapshotMessage
record
