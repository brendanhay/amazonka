{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeSnapshots
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

-- | Describes one or more of the EBS snapshots available to you. Available
-- snapshots include public snapshots available for any AWS account to
-- launch, private snapshots that you own, and private snapshots owned by
-- another AWS account but for which you\'ve been given explicit create
-- volume permissions.
--
-- The create volume permissions fall into the following categories:
--
-- -   /public/: The owner of the snapshot granted create volume
--     permissions for the snapshot to the @all@ group. All AWS accounts
--     have create volume permissions for these snapshots.
-- -   /explicit/: The owner of the snapshot granted create volume
--     permissions to a specific AWS account.
-- -   /implicit/: An AWS account has implicit create volume permissions
--     for all snapshots it owns.
--
-- The list of snapshots returned can be modified by specifying snapshot
-- IDs, snapshot owners, or AWS accounts with create volume permissions. If
-- no options are specified, Amazon EC2 returns all snapshots for which you
-- have create volume permissions.
--
-- If you specify one or more snapshot IDs, only snapshots that have the
-- specified IDs are returned. If you specify an invalid snapshot ID, an
-- error is returned. If you specify a snapshot ID for which you do not
-- have access, it is not included in the returned results.
--
-- If you specify one or more snapshot owners, only snapshots from the
-- specified owners and for which you have access are returned. The results
-- can include the AWS account IDs of the specified owners, @amazon@ for
-- snapshots owned by Amazon, or @self@ for snapshots that you own.
--
-- If you specify a list of restorable users, only snapshots with create
-- snapshot permissions for those users are returned. You can specify AWS
-- account IDs (if you own the snapshots), @self@ for snapshots for which
-- you own or have explicit permissions, or @all@ for public snapshots.
--
-- If you are describing a long list of snapshots, you can paginate the
-- output to make the list more manageable. The @MaxResults@ parameter sets
-- the maximum number of results returned in a single page. If the list of
-- results exceeds your @MaxResults@ value, then that number of results is
-- returned along with a @NextToken@ value that can be passed to a
-- subsequent @DescribeSnapshots@ request to retrieve the remaining
-- results.
--
-- For more information about EBS snapshots, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html>
module Network.AWS.EC2.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , ds1OwnerIds
    , ds1Filters
    , ds1NextToken
    , ds1SnapshotIds
    , ds1RestorableByUserIds
    , ds1DryRun
    , ds1MaxResults

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsr1NextToken
    , dsr1Snapshots
    , dsr1Status
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1OwnerIds'
--
-- * 'ds1Filters'
--
-- * 'ds1NextToken'
--
-- * 'ds1SnapshotIds'
--
-- * 'ds1RestorableByUserIds'
--
-- * 'ds1DryRun'
--
-- * 'ds1MaxResults'
data DescribeSnapshots = DescribeSnapshots'
    { _ds1OwnerIds            :: !(Maybe [Text])
    , _ds1Filters             :: !(Maybe [Filter])
    , _ds1NextToken           :: !(Maybe Text)
    , _ds1SnapshotIds         :: !(Maybe [Text])
    , _ds1RestorableByUserIds :: !(Maybe [Text])
    , _ds1DryRun              :: !(Maybe Bool)
    , _ds1MaxResults          :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeSnapshots' smart constructor.
describeSnapshots :: DescribeSnapshots
describeSnapshots =
    DescribeSnapshots'
    { _ds1OwnerIds = Nothing
    , _ds1Filters = Nothing
    , _ds1NextToken = Nothing
    , _ds1SnapshotIds = Nothing
    , _ds1RestorableByUserIds = Nothing
    , _ds1DryRun = Nothing
    , _ds1MaxResults = Nothing
    }

-- | Returns the snapshots owned by the specified owner. Multiple owners can
-- be specified.
ds1OwnerIds :: Lens' DescribeSnapshots [Text]
ds1OwnerIds = lens _ds1OwnerIds (\ s a -> s{_ds1OwnerIds = a}) . _Default;

-- | One or more filters.
--
-- -   @description@ - A description of the snapshot.
--
-- -   @owner-alias@ - The AWS account alias (for example, @amazon@) that
--     owns the snapshot.
--
-- -   @owner-id@ - The ID of the AWS account that owns the snapshot.
--
-- -   @progress@ - The progress of the snapshot, as a percentage (for
--     example, 80%).
--
-- -   @snapshot-id@ - The snapshot ID.
--
-- -   @start-time@ - The time stamp when the snapshot was initiated.
--
-- -   @status@ - The status of the snapshot (@pending@ | @completed@ |
--     @error@).
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
-- -   @volume-id@ - The ID of the volume the snapshot is for.
--
-- -   @volume-size@ - The size of the volume, in GiB.
--
ds1Filters :: Lens' DescribeSnapshots [Filter]
ds1Filters = lens _ds1Filters (\ s a -> s{_ds1Filters = a}) . _Default;

-- | The @NextToken@ value returned from a previous paginated
-- @DescribeSnapshots@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
ds1NextToken :: Lens' DescribeSnapshots (Maybe Text)
ds1NextToken = lens _ds1NextToken (\ s a -> s{_ds1NextToken = a});

-- | One or more snapshot IDs.
--
-- Default: Describes snapshots for which you have launch permissions.
ds1SnapshotIds :: Lens' DescribeSnapshots [Text]
ds1SnapshotIds = lens _ds1SnapshotIds (\ s a -> s{_ds1SnapshotIds = a}) . _Default;

-- | One or more AWS accounts IDs that can create volumes from the snapshot.
ds1RestorableByUserIds :: Lens' DescribeSnapshots [Text]
ds1RestorableByUserIds = lens _ds1RestorableByUserIds (\ s a -> s{_ds1RestorableByUserIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ds1DryRun :: Lens' DescribeSnapshots (Maybe Bool)
ds1DryRun = lens _ds1DryRun (\ s a -> s{_ds1DryRun = a});

-- | The maximum number of snapshot results returned by @DescribeSnapshots@
-- in paginated output. When this parameter is used, @DescribeSnapshots@
-- only returns @MaxResults@ results in a single page along with a
-- @NextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeSnapshots@ request with
-- the returned @NextToken@ value. This value can be between 5 and 1000; if
-- @MaxResults@ is given a value larger than 1000, only 1000 results are
-- returned. If this parameter is not used, then @DescribeSnapshots@
-- returns all results. You cannot specify this parameter and the snapshot
-- IDs parameter in the same request.
ds1MaxResults :: Lens' DescribeSnapshots (Maybe Int)
ds1MaxResults = lens _ds1MaxResults (\ s a -> s{_ds1MaxResults = a});

instance AWSPager DescribeSnapshots where
        page rq rs
          | stop (rs ^. dsr1NextToken) = Nothing
          | stop (rs ^. dsr1Snapshots) = Nothing
          | otherwise =
            Just $ rq & ds1NextToken .~ rs ^. dsr1NextToken

instance AWSRequest DescribeSnapshots where
        type Sv DescribeSnapshots = EC2
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "snapshotSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeSnapshots where
        toHeaders = const mempty

instance ToPath DescribeSnapshots where
        toPath = const "/"

instance ToQuery DescribeSnapshots where
        toQuery DescribeSnapshots'{..}
          = mconcat
              ["Action" =: ("DescribeSnapshots" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Owner" <$> _ds1OwnerIds),
               toQuery (toQueryList "Filter" <$> _ds1Filters),
               "NextToken" =: _ds1NextToken,
               toQuery
                 (toQueryList "SnapshotId" <$> _ds1SnapshotIds),
               toQuery
                 (toQueryList "RestorableBy" <$>
                    _ds1RestorableByUserIds),
               "DryRun" =: _ds1DryRun,
               "MaxResults" =: _ds1MaxResults]

-- | /See:/ 'describeSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsr1NextToken'
--
-- * 'dsr1Snapshots'
--
-- * 'dsr1Status'
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
    { _dsr1NextToken :: !(Maybe Text)
    , _dsr1Snapshots :: !(Maybe [Snapshot])
    , _dsr1Status    :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeSnapshotsResponse' smart constructor.
describeSnapshotsResponse :: Int -> DescribeSnapshotsResponse
describeSnapshotsResponse pStatus =
    DescribeSnapshotsResponse'
    { _dsr1NextToken = Nothing
    , _dsr1Snapshots = Nothing
    , _dsr1Status = pStatus
    }

-- | The @NextToken@ value to include in a future @DescribeSnapshots@
-- request. When the results of a @DescribeSnapshots@ request exceed
-- @MaxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
dsr1NextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
dsr1NextToken = lens _dsr1NextToken (\ s a -> s{_dsr1NextToken = a});

-- | Information about the snapshots.
dsr1Snapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsr1Snapshots = lens _dsr1Snapshots (\ s a -> s{_dsr1Snapshots = a}) . _Default;

-- | FIXME: Undocumented member.
dsr1Status :: Lens' DescribeSnapshotsResponse Int
dsr1Status = lens _dsr1Status (\ s a -> s{_dsr1Status = a});
