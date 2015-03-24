{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of the Amazon EBS snapshots available to you. Available
-- snapshots include public snapshots available for any AWS account to launch,
-- private snapshots that you own, and private snapshots owned by another AWS
-- account but for which you've been given explicit create volume permissions.
--
-- The create volume permissions fall into the following categories:
--
-- /public/: The owner of the snapshot granted create volume permissions for
-- the snapshot to the 'all' group. All AWS accounts have create volume
-- permissions for these snapshots.  /explicit/: The owner of the snapshot granted
-- create volume permissions to a specific AWS account.  /implicit/: An AWS
-- account has implicit create volume permissions for all snapshots it owns.  The list of snapshots returned can be modified by specifying snapshot IDs, snapshot owners, or AWS accounts with create volume permissions. If no options are specified, Amazon EC2 returns all snapshots for which you have create volume permissions.
--
--
-- If you specify one or more snapshot IDs, only snapshots that have the
-- specified IDs are returned. If you specify an invalid snapshot ID, an error
-- is returned. If you specify a snapshot ID for which you do not have access,
-- it is not included in the returned results.
--
-- If you specify one or more snapshot owners, only snapshots from the
-- specified owners and for which you have access are returned. The results can
-- include the AWS account IDs of the specified owners, 'amazon' for snapshots
-- owned by Amazon, or 'self' for snapshots that you own.
--
-- If you specify a list of restorable users, only snapshots with create
-- snapshot permissions for those users are returned. You can specify AWS
-- account IDs (if you own the snapshots), 'self' for snapshots for which you own
-- or have explicit permissions, or 'all' for public snapshots.
--
-- If you are describing a long list of snapshots, you can paginate the output
-- to make the list more manageable. The 'MaxResults' parameter sets the maximum
-- number of results returned in a single page. If the list of results exceeds
-- your 'MaxResults' value, then that number of results is returned along with a 'NextToken' value that can be passed to a subsequent 'DescribeSnapshots' request to
-- retrieve the remaining results.
--
-- For more information about Amazon EBS snapshots, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots> in
-- the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html>
module Network.AWS.EC2.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , ds1DryRun
    , ds1Filters
    , ds1MaxResults
    , ds1NextToken
    , ds1OwnerIds
    , ds1RestorableByUserIds
    , ds1SnapshotIds

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsrNextToken
    , dsrSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSnapshots = DescribeSnapshots
    { _ds1DryRun              :: Maybe Bool
    , _ds1Filters             :: List "Filter" Filter
    , _ds1MaxResults          :: Maybe Int
    , _ds1NextToken           :: Maybe Text
    , _ds1OwnerIds            :: List "Owner" Text
    , _ds1RestorableByUserIds :: List "RestorableBy" Text
    , _ds1SnapshotIds         :: List "SnapshotId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeSnapshots' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ds1Filters' @::@ ['Filter']
--
-- * 'ds1MaxResults' @::@ 'Maybe' 'Int'
--
-- * 'ds1NextToken' @::@ 'Maybe' 'Text'
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
    , _ds1NextToken           = Nothing
    , _ds1MaxResults          = Nothing
    }

ds1DryRun :: Lens' DescribeSnapshots (Maybe Bool)
ds1DryRun = lens _ds1DryRun (\s a -> s { _ds1DryRun = a })

-- | One or more filters.
--
-- 'description' - A description of the snapshot.
--
-- 'owner-alias' - The AWS account alias (for example, 'amazon') that owns the
-- snapshot.
--
-- 'owner-id' - The ID of the AWS account that owns the snapshot.
--
-- 'progress' - The progress of the snapshot, as a percentage (for example,
-- 80%).
--
-- 'snapshot-id' - The snapshot ID.
--
-- 'start-time' - The time stamp when the snapshot was initiated.
--
-- 'status' - The status of the snapshot ('pending' | 'completed' | 'error').
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the 'tag-value' filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
-- 'volume-id' - The ID of the volume the snapshot is for.
--
-- 'volume-size' - The size of the volume, in GiB.
--
--
ds1Filters :: Lens' DescribeSnapshots [Filter]
ds1Filters = lens _ds1Filters (\s a -> s { _ds1Filters = a }) . _List

-- | The maximum number of snapshot results returned by 'DescribeSnapshots' in
-- paginated output. When this parameter is used, 'DescribeSnapshots' only returns 'MaxResults' results in a single page along with a 'NextToken' response element.
-- The remaining results of the initial request can be seen by sending another 'DescribeSnapshots' request with the returned 'NextToken' value. This value can be between 5 and
-- 1000; if 'MaxResults' is given a value larger than 1000, only 1000 results are
-- returned. If this parameter is not used, then 'DescribeSnapshots' returns all
-- results. You cannot specify this parameter and the snapshot IDs parameter in
-- the same request.
ds1MaxResults :: Lens' DescribeSnapshots (Maybe Int)
ds1MaxResults = lens _ds1MaxResults (\s a -> s { _ds1MaxResults = a })

-- | The 'NextToken' value returned from a previous paginated 'DescribeSnapshots'
-- request where 'MaxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'NextToken' value. This value is 'null' when there are no more
-- results to return.
ds1NextToken :: Lens' DescribeSnapshots (Maybe Text)
ds1NextToken = lens _ds1NextToken (\s a -> s { _ds1NextToken = a })

-- | Returns the snapshots owned by the specified owner. Multiple owners can be
-- specified.
ds1OwnerIds :: Lens' DescribeSnapshots [Text]
ds1OwnerIds = lens _ds1OwnerIds (\s a -> s { _ds1OwnerIds = a }) . _List

-- | One or more AWS accounts IDs that can create volumes from the snapshot.
ds1RestorableByUserIds :: Lens' DescribeSnapshots [Text]
ds1RestorableByUserIds =
    lens _ds1RestorableByUserIds (\s a -> s { _ds1RestorableByUserIds = a })
        . _List

-- | One or more snapshot IDs.
--
-- Default: Describes snapshots for which you have launch permissions.
ds1SnapshotIds :: Lens' DescribeSnapshots [Text]
ds1SnapshotIds = lens _ds1SnapshotIds (\s a -> s { _ds1SnapshotIds = a }) . _List

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrNextToken :: Maybe Text
    , _dsrSnapshots :: List "item" Snapshot
    } deriving (Eq, Read, Show)

-- | 'DescribeSnapshotsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsrSnapshots' @::@ ['Snapshot']
--
describeSnapshotsResponse :: DescribeSnapshotsResponse
describeSnapshotsResponse = DescribeSnapshotsResponse
    { _dsrSnapshots = mempty
    , _dsrNextToken = Nothing
    }

-- | The 'NextToken' value to include in a future 'DescribeSnapshots' request. When
-- the results of a 'DescribeSnapshots' request exceed 'MaxResults', this value can
-- be used to retrieve the next page of results. This value is 'null' when there
-- are no more results to return.
dsrNextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
dsrNextToken = lens _dsrNextToken (\s a -> s { _dsrNextToken = a })

dsrSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsrSnapshots = lens _dsrSnapshots (\s a -> s { _dsrSnapshots = a }) . _List

instance ToPath DescribeSnapshots where
    toPath = const "/"

instance ToQuery DescribeSnapshots where
    toQuery DescribeSnapshots{..} = mconcat
        [ "DryRun"       =? _ds1DryRun
        , "Filter"       `toQueryList` _ds1Filters
        , "MaxResults"   =? _ds1MaxResults
        , "NextToken"    =? _ds1NextToken
        , "Owner"        `toQueryList` _ds1OwnerIds
        , "RestorableBy" `toQueryList` _ds1RestorableByUserIds
        , "SnapshotId"   `toQueryList` _ds1SnapshotIds
        ]

instance ToHeaders DescribeSnapshots

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = EC2
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request  = post "DescribeSnapshots"
    response = xmlResponse

instance FromXML DescribeSnapshotsResponse where
    parseXML x = DescribeSnapshotsResponse
        <$> x .@? "nextToken"
        <*> x .@? "snapshotSet" .!@ mempty

instance AWSPager DescribeSnapshots where
    page rq rs
        | stop (rs ^. dsrNextToken) = Nothing
        | otherwise = (\x -> rq & ds1NextToken ?~ x)
            <$> (rs ^. dsrNextToken)
