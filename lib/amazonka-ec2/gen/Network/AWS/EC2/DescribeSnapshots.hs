{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the EBS snapshots available to you. Available snapshots include public snapshots available for any AWS account to launch, private snapshots that you own, and private snapshots owned by another AWS account but for which you've been given explicit create volume permissions.
--
--
-- The create volume permissions fall into the following categories:
--
--     * /public/ : The owner of the snapshot granted create volume permissions for the snapshot to the @all@ group. All AWS accounts have create volume permissions for these snapshots.
--
--     * /explicit/ : The owner of the snapshot granted create volume permissions to a specific AWS account.
--
--     * /implicit/ : An AWS account has implicit create volume permissions for all snapshots it owns.
--
--
--
-- The list of snapshots returned can be modified by specifying snapshot IDs, snapshot owners, or AWS accounts with create volume permissions. If no options are specified, Amazon EC2 returns all snapshots for which you have create volume permissions.
--
-- If you specify one or more snapshot IDs, only snapshots that have the specified IDs are returned. If you specify an invalid snapshot ID, an error is returned. If you specify a snapshot ID for which you do not have access, it is not included in the returned results.
--
-- If you specify one or more snapshot owners using the @OwnerIds@ option, only snapshots from the specified owners and for which you have access are returned. The results can include the AWS account IDs of the specified owners, @amazon@ for snapshots owned by Amazon, or @self@ for snapshots that you own.
--
-- If you specify a list of restorable users, only snapshots with create snapshot permissions for those users are returned. You can specify AWS account IDs (if you own the snapshots), @self@ for snapshots for which you own or have explicit permissions, or @all@ for public snapshots.
--
-- If you are describing a long list of snapshots, you can paginate the output to make the list more manageable. The @MaxResults@ parameter sets the maximum number of results returned in a single page. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeSnapshots@ request to retrieve the remaining results.
--
-- For more information about EBS snapshots, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSnapshots
    (
    -- * Creating a Request
      describeSnapshots
    , DescribeSnapshots
    -- * Request Lenses
    , dssOwnerIds
    , dssFilters
    , dssNextToken
    , dssSnapshotIds
    , dssRestorableByUserIds
    , dssDryRun
    , dssMaxResults

    -- * Destructuring the Response
    , describeSnapshotsResponse
    , DescribeSnapshotsResponse
    -- * Response Lenses
    , dssrsNextToken
    , dssrsSnapshots
    , dssrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSnapshots.
--
--
--
-- /See:/ 'describeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { _dssOwnerIds            :: !(Maybe [Text])
  , _dssFilters             :: !(Maybe [Filter])
  , _dssNextToken           :: !(Maybe Text)
  , _dssSnapshotIds         :: !(Maybe [Text])
  , _dssRestorableByUserIds :: !(Maybe [Text])
  , _dssDryRun              :: !(Maybe Bool)
  , _dssMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssOwnerIds' - Returns the snapshots owned by the specified owner. Multiple owners can be specified.
--
-- * 'dssFilters' - One or more filters.     * @description@ - A description of the snapshot.     * @owner-alias@ - Value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.     * @owner-id@ - The ID of the AWS account that owns the snapshot.     * @progress@ - The progress of the snapshot, as a percentage (for example, 80%).     * @snapshot-id@ - The snapshot ID.     * @start-time@ - The time stamp when the snapshot was initiated.     * @status@ - The status of the snapshot (@pending@ | @completed@ | @error@ ).     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @volume-id@ - The ID of the volume the snapshot is for.     * @volume-size@ - The size of the volume, in GiB.
--
-- * 'dssNextToken' - The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'dssSnapshotIds' - One or more snapshot IDs. Default: Describes snapshots for which you have launch permissions.
--
-- * 'dssRestorableByUserIds' - One or more AWS accounts IDs that can create volumes from the snapshot.
--
-- * 'dssDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dssMaxResults' - The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
describeSnapshots
    :: DescribeSnapshots
describeSnapshots =
  DescribeSnapshots'
    { _dssOwnerIds = Nothing
    , _dssFilters = Nothing
    , _dssNextToken = Nothing
    , _dssSnapshotIds = Nothing
    , _dssRestorableByUserIds = Nothing
    , _dssDryRun = Nothing
    , _dssMaxResults = Nothing
    }


-- | Returns the snapshots owned by the specified owner. Multiple owners can be specified.
dssOwnerIds :: Lens' DescribeSnapshots [Text]
dssOwnerIds = lens _dssOwnerIds (\ s a -> s{_dssOwnerIds = a}) . _Default . _Coerce

-- | One or more filters.     * @description@ - A description of the snapshot.     * @owner-alias@ - Value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.     * @owner-id@ - The ID of the AWS account that owns the snapshot.     * @progress@ - The progress of the snapshot, as a percentage (for example, 80%).     * @snapshot-id@ - The snapshot ID.     * @start-time@ - The time stamp when the snapshot was initiated.     * @status@ - The status of the snapshot (@pending@ | @completed@ | @error@ ).     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @volume-id@ - The ID of the volume the snapshot is for.     * @volume-size@ - The size of the volume, in GiB.
dssFilters :: Lens' DescribeSnapshots [Filter]
dssFilters = lens _dssFilters (\ s a -> s{_dssFilters = a}) . _Default . _Coerce

-- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
dssNextToken :: Lens' DescribeSnapshots (Maybe Text)
dssNextToken = lens _dssNextToken (\ s a -> s{_dssNextToken = a})

-- | One or more snapshot IDs. Default: Describes snapshots for which you have launch permissions.
dssSnapshotIds :: Lens' DescribeSnapshots [Text]
dssSnapshotIds = lens _dssSnapshotIds (\ s a -> s{_dssSnapshotIds = a}) . _Default . _Coerce

-- | One or more AWS accounts IDs that can create volumes from the snapshot.
dssRestorableByUserIds :: Lens' DescribeSnapshots [Text]
dssRestorableByUserIds = lens _dssRestorableByUserIds (\ s a -> s{_dssRestorableByUserIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dssDryRun :: Lens' DescribeSnapshots (Maybe Bool)
dssDryRun = lens _dssDryRun (\ s a -> s{_dssDryRun = a})

-- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
dssMaxResults :: Lens' DescribeSnapshots (Maybe Int)
dssMaxResults = lens _dssMaxResults (\ s a -> s{_dssMaxResults = a})

instance AWSPager DescribeSnapshots where
        page rq rs
          | stop (rs ^. dssrsNextToken) = Nothing
          | stop (rs ^. dssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dssNextToken .~ rs ^. dssrsNextToken

instance AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "snapshotSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSnapshots where

instance NFData DescribeSnapshots where

instance ToHeaders DescribeSnapshots where
        toHeaders = const mempty

instance ToPath DescribeSnapshots where
        toPath = const "/"

instance ToQuery DescribeSnapshots where
        toQuery DescribeSnapshots'{..}
          = mconcat
              ["Action" =: ("DescribeSnapshots" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Owner" <$> _dssOwnerIds),
               toQuery (toQueryList "Filter" <$> _dssFilters),
               "NextToken" =: _dssNextToken,
               toQuery
                 (toQueryList "SnapshotId" <$> _dssSnapshotIds),
               toQuery
                 (toQueryList "RestorableBy" <$>
                    _dssRestorableByUserIds),
               "DryRun" =: _dssDryRun,
               "MaxResults" =: _dssMaxResults]

-- | Contains the output of DescribeSnapshots.
--
--
--
-- /See:/ 'describeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { _dssrsNextToken      :: !(Maybe Text)
  , _dssrsSnapshots      :: !(Maybe [Snapshot])
  , _dssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsNextToken' - The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dssrsSnapshots' - Information about the snapshots.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSnapshotsResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSnapshotsResponse
describeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { _dssrsNextToken = Nothing
    , _dssrsSnapshots = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
dssrsNextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
dssrsNextToken = lens _dssrsNextToken (\ s a -> s{_dssrsNextToken = a})

-- | Information about the snapshots.
dssrsSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dssrsSnapshots = lens _dssrsSnapshots (\ s a -> s{_dssrsSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSnapshotsResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeSnapshotsResponse where
