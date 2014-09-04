{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSnapshots
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
-- Compute Cloud User Guide. Example This example describes a snapshot with an
-- ID of snap-1a2b3c4d. https://ec2.amazonaws.com/?Action=DescribeSnapshots
-- &amp;SnapshotId=snap-1a2b3c4d &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d vol-1a2b3c4d pending
-- YYYY-MM-DDTHH:MM:SS.SSSZ 80% 111122223333 15 Daily Backup true Example This
-- example filters the response to include only snapshots with the pending
-- status, and that are also tagged with a value that includes the string db_.
-- https://ec2.amazonaws.com/?Action=DescribeSnapshots
-- &amp;Filter.1.Name=status &amp;Filter.1.Value.1=pending
-- &amp;Filter.2.Name=tag-value &amp;Filter.2.Value.1=*db_* &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d vol-1a2b3c4d pending
-- YYYY-MM-DDTHH:MM:SS.SSSZ 30% 111122223333 15 Daily Backup Purpose
-- demo_db_14_backup true.
module Network.AWS.EC2.V2014_06_15.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , dsuFilters
    , dsuOwnerIds
    , dsuRestorableByUserIds
    , dsuSnapshotIds

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response lenses
    , dsvSnapshots
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSnapshots' request.
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots
    { _dsuFilters = mempty
    , _dsuOwnerIds = mempty
    , _dsuRestorableByUserIds = mempty
    , _dsuSnapshotIds = mempty
    }
{-# INLINE describeSnapshots #-}

data DescribeSnapshots = DescribeSnapshots
    { _dsuFilters :: [Filter]
      -- ^ One or more filters. description - A description of the snapshot.
      -- owner-alias - The AWS account alias (for example, amazon) that
      -- owns the snapshot. owner-id - The ID of the AWS account that owns
      -- the snapshot. progress - The progress of the snapshot, as a
      -- percentage (for example, 80%). snapshot-id - The snapshot ID.
      -- start-time - The time stamp when the snapshot was initiated.
      -- status - The status of the snapshot (pending | completed |
      -- error). tag:key=value - The key/value combination of a tag
      -- assigned to the resource. tag-key - The key of a tag assigned to
      -- the resource. This filter is independent of the tag-value filter.
      -- For example, if you use both the filter "tag-key=Purpose" and the
      -- filter "tag-value=X", you get any resources assigned both the tag
      -- key Purpose (regardless of what the tag's value is), and the tag
      -- value X (regardless of what the tag's key is). If you want to
      -- list only resources where Purpose is X, see the tag:key=value
      -- filter. tag-value - The value of a tag assigned to the resource.
      -- This filter is independent of the tag-key filter. volume-id - The
      -- ID of the volume the snapshot is for. volume-size - The size of
      -- the volume, in GiB.
    , _dsuOwnerIds :: [Text]
      -- ^ Returns the snapshots owned by the specified owner. Multiple
      -- owners can be specified.
    , _dsuRestorableByUserIds :: [Text]
      -- ^ One or more AWS accounts IDs that can create volumes from the
      -- snapshot.
    , _dsuSnapshotIds :: [Text]
      -- ^ One or more snapshot IDs. Default: Describes snapshots for which
      -- you have launch permissions.
    } deriving (Show, Generic)

-- | One or more filters. description - A description of the snapshot.
-- owner-alias - The AWS account alias (for example, amazon) that owns the
-- snapshot. owner-id - The ID of the AWS account that owns the snapshot.
-- progress - The progress of the snapshot, as a percentage (for example,
-- 80%). snapshot-id - The snapshot ID. start-time - The time stamp when the
-- snapshot was initiated. status - The status of the snapshot (pending |
-- completed | error). tag:key=value - The key/value combination of a tag
-- assigned to the resource. tag-key - The key of a tag assigned to the
-- resource. This filter is independent of the tag-value filter. For example,
-- if you use both the filter "tag-key=Purpose" and the filter "tag-value=X",
-- you get any resources assigned both the tag key Purpose (regardless of what
-- the tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter. volume-id - The
-- ID of the volume the snapshot is for. volume-size - The size of the volume,
-- in GiB.
dsuFilters :: Lens' DescribeSnapshots [Filter]
dsuFilters f x =
    f (_dsuFilters x) <&> \y -> x { _dsuFilters = y }
{-# INLINE dsuFilters #-}

-- | Returns the snapshots owned by the specified owner. Multiple owners can be
-- specified.
dsuOwnerIds :: Lens' DescribeSnapshots [Text]
dsuOwnerIds f x =
    f (_dsuOwnerIds x) <&> \y -> x { _dsuOwnerIds = y }
{-# INLINE dsuOwnerIds #-}

-- | One or more AWS accounts IDs that can create volumes from the snapshot.
dsuRestorableByUserIds :: Lens' DescribeSnapshots [Text]
dsuRestorableByUserIds f x =
    f (_dsuRestorableByUserIds x) <&> \y -> x { _dsuRestorableByUserIds = y }
{-# INLINE dsuRestorableByUserIds #-}

-- | One or more snapshot IDs. Default: Describes snapshots for which you have
-- launch permissions.
dsuSnapshotIds :: Lens' DescribeSnapshots [Text]
dsuSnapshotIds f x =
    f (_dsuSnapshotIds x) <&> \y -> x { _dsuSnapshotIds = y }
{-# INLINE dsuSnapshotIds #-}

instance ToQuery DescribeSnapshots where
    toQuery = genericQuery def

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsvSnapshots :: [Snapshot]
      -- ^ 
    } deriving (Show, Generic)

-- | 
dsvSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsvSnapshots f x =
    f (_dsvSnapshots x) <&> \y -> x { _dsvSnapshots = y }
{-# INLINE dsvSnapshots #-}

instance FromXML DescribeSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = EC2
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request = post "DescribeSnapshots"
    response _ = xmlResponse
