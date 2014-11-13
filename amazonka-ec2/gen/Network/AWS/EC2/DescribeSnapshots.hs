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

-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the Amazon EBS snapshots available to you.
-- Available snapshots include public snapshots available for any AWS account
-- to launch, private snapshots that you own, and private snapshots owned by
-- another AWS account but for which you've been given explicit create volume
-- permissions. The create volume permissions fall into the following
-- categories: public: The owner of the snapshot granted create volume
-- permissions for the snapshot to the all group. All AWS accounts have create
-- volume permissions for these snapshots. explicit: The owner of the snapshot
-- granted create volume permissions to a specific AWS account. implicit: An
-- AWS account has implicit create volume permissions for all snapshots it
-- owns. The list of snapshots returned can be modified by specifying snapshot
-- IDs, snapshot owners, or AWS accounts with create volume permissions. If no
-- options are specified, Amazon EC2 returns all snapshots for which you have
-- create volume permissions. If you specify one or more snapshot IDs, only
-- snapshots that have the specified IDs are returned. If you specify an
-- invalid snapshot ID, an error is returned. If you specify a snapshot ID for
-- which you do not have access, it is not included in the returned results.
-- If you specify one or more snapshot owners, only snapshots from the
-- specified owners and for which you have access are returned. The results
-- can include the AWS account IDs of the specified owners, amazon for
-- snapshots owned by Amazon, or self for snapshots that you own. If you
-- specify a list of restorable users, only snapshots with create snapshot
-- permissions for those users are returned. You can specify AWS account IDs
-- (if you own the snapshots), self for snapshots for which you own or have
-- explicit permissions, or all for public snapshots. For more information
-- about Amazon EBS snapshots, see Amazon EBS Snapshots in the Amazon Elastic
-- Compute Cloud User Guide.
module Network.AWS.EC2.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , ds1DryRun
    , ds1Filters
    , ds1OwnerIds
    , ds1RestorableByUserIds
    , ds1SnapshotIds

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsrSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSnapshots = DescribeSnapshots
    { _ds1DryRun              :: Maybe Bool
    , _ds1Filters             :: [Filter]
    , _ds1OwnerIds            :: [Text]
    , _ds1RestorableByUserIds :: [Text]
    , _ds1SnapshotIds         :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeSnapshots' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ds1Filters' @::@ ['Filter']
--
-- * 'ds1OwnerIds' @::@ ['Text']
--
-- * 'ds1RestorableByUserIds' @::@ ['Text']
--
-- * 'ds1SnapshotIds' @::@ ['Text']
--
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots
    { _ds1DryRun              = Nothing
    , _ds1SnapshotIds         = mempty
    , _ds1OwnerIds            = mempty
    , _ds1RestorableByUserIds = mempty
    , _ds1Filters             = mempty
    }

ds1DryRun :: Lens' DescribeSnapshots (Maybe Bool)
ds1DryRun = lens _ds1DryRun (\s a -> s { _ds1DryRun = a })

-- | One or more filters. description - A description of the snapshot.
-- owner-alias - The AWS account alias (for example, amazon) that owns the
-- snapshot. owner-id - The ID of the AWS account that owns the snapshot.
-- progress - The progress of the snapshot, as a percentage (for example,
-- 80%). snapshot-id - The snapshot ID. start-time - The time stamp when the
-- snapshot was initiated. status - The status of the snapshot (pending |
-- completed | error). tag:key=value - The key/value combination of a tag
-- assigned to the resource. tag-key - The key of a tag assigned to the
-- resource. This filter is independent of the tag-value filter. For
-- example, if you use both the filter "tag-key=Purpose" and the filter
-- "tag-value=X", you get any resources assigned both the tag key Purpose
-- (regardless of what the tag's value is), and the tag value X (regardless
-- of what the tag's key is). If you want to list only resources where
-- Purpose is X, see the tag:key=value filter. tag-value - The value of a
-- tag assigned to the resource. This filter is independent of the tag-key
-- filter. volume-id - The ID of the volume the snapshot is for. volume-size
-- - The size of the volume, in GiB.
ds1Filters :: Lens' DescribeSnapshots [Filter]
ds1Filters = lens _ds1Filters (\s a -> s { _ds1Filters = a })

-- | Returns the snapshots owned by the specified owner. Multiple owners can
-- be specified.
ds1OwnerIds :: Lens' DescribeSnapshots [Text]
ds1OwnerIds = lens _ds1OwnerIds (\s a -> s { _ds1OwnerIds = a })

-- | One or more AWS accounts IDs that can create volumes from the snapshot.
ds1RestorableByUserIds :: Lens' DescribeSnapshots [Text]
ds1RestorableByUserIds =
    lens _ds1RestorableByUserIds (\s a -> s { _ds1RestorableByUserIds = a })

-- | One or more snapshot IDs. Default: Describes snapshots for which you have
-- launch permissions.
ds1SnapshotIds :: Lens' DescribeSnapshots [Text]
ds1SnapshotIds = lens _ds1SnapshotIds (\s a -> s { _ds1SnapshotIds = a })

instance ToQuery DescribeSnapshots

instance ToPath DescribeSnapshots where
    toPath = const "/"

newtype DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrSnapshots :: [Snapshot]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeSnapshotsResponse where
    type Item DescribeSnapshotsResponse = Snapshot

    fromList = DescribeSnapshotsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsrSnapshots

-- | 'DescribeSnapshotsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSnapshots' @::@ ['Snapshot']
--
describeSnapshotsResponse :: DescribeSnapshotsResponse
describeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrSnapshots = mempty
    }

dsrSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsrSnapshots = lens _dsrSnapshots (\s a -> s { _dsrSnapshots = a })

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = EC2
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request  = post "DescribeSnapshots"
    response = xmlResponse $ \h x -> DescribeSnapshotsResponse
        <$> x %| "snapshotSet"
