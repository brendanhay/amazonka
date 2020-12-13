{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS snapshots available to you or all of the EBS snapshots available to you.
--
-- The snapshots available to you include public snapshots, private snapshots that you own, and private snapshots owned by other AWS accounts for which you have explicit create volume permissions.
-- The create volume permissions fall into the following categories:
--
--     * /public/ : The owner of the snapshot granted create volume permissions for the snapshot to the @all@ group. All AWS accounts have create volume permissions for these snapshots.
--
--
--     * /explicit/ : The owner of the snapshot granted create volume permissions to a specific AWS account.
--
--
--     * /implicit/ : An AWS account has implicit create volume permissions for all snapshots it owns.
--
--
-- The list of snapshots returned can be filtered by specifying snapshot IDs, snapshot owners, or AWS accounts with create volume permissions. If no options are specified, Amazon EC2 returns all snapshots for which you have create volume permissions.
-- If you specify one or more snapshot IDs, only snapshots that have the specified IDs are returned. If you specify an invalid snapshot ID, an error is returned. If you specify a snapshot ID for which you do not have access, it is not included in the returned results.
-- If you specify one or more snapshot owners using the @OwnerIds@ option, only snapshots from the specified owners and for which you have access are returned. The results can include the AWS account IDs of the specified owners, @amazon@ for snapshots owned by Amazon, or @self@ for snapshots that you own.
-- If you specify a list of restorable users, only snapshots with create snapshot permissions for those users are returned. You can specify AWS account IDs (if you own the snapshots), @self@ for snapshots for which you own or have explicit permissions, or @all@ for public snapshots.
-- If you are describing a long list of snapshots, we recommend that you paginate the output to make the list more manageable. The @MaxResults@ parameter sets the maximum number of results returned in a single page. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeSnapshots@ request to retrieve the remaining results.
-- To get the state of fast snapshot restores for a snapshot, use 'DescribeFastSnapshotRestores' .
-- For more information about EBS snapshots, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSnapshots
  ( -- * Creating a request
    DescribeSnapshots (..),
    mkDescribeSnapshots,

    -- ** Request lenses
    dssOwnerIds,
    dssFilters,
    dssNextToken,
    dssSnapshotIds,
    dssRestorableByUserIds,
    dssDryRun,
    dssMaxResults,

    -- * Destructuring the response
    DescribeSnapshotsResponse (..),
    mkDescribeSnapshotsResponse,

    -- ** Response lenses
    dssrsNextToken,
    dssrsSnapshots,
    dssrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
    ownerIds :: Lude.Maybe [Lude.Text],
    -- | The filters.
    --
    --
    --     * @description@ - A description of the snapshot.
    --
    --
    --     * @encrypted@ - Indicates whether the snapshot is encrypted (@true@ | @false@ )
    --
    --
    --     * @owner-alias@ - The owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console. We recommend that you use the related parameter instead of this filter.
    --
    --
    --     * @owner-id@ - The AWS account ID of the owner. We recommend that you use the related parameter instead of this filter.
    --
    --
    --     * @progress@ - The progress of the snapshot, as a percentage (for example, 80%).
    --
    --
    --     * @snapshot-id@ - The snapshot ID.
    --
    --
    --     * @start-time@ - The time stamp when the snapshot was initiated.
    --
    --
    --     * @status@ - The status of the snapshot (@pending@ | @completed@ | @error@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @volume-id@ - The ID of the volume the snapshot is for.
    --
    --
    --     * @volume-size@ - The size of the volume, in GiB.
    filters :: Lude.Maybe [Filter],
    -- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The snapshot IDs.
    --
    -- Default: Describes the snapshots for which you have create volume permissions.
    snapshotIds :: Lude.Maybe [Lude.Text],
    -- | The IDs of the AWS accounts that can create volumes from the snapshot.
    restorableByUserIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- * 'ownerIds' - Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
-- * 'filters' - The filters.
--
--
--     * @description@ - A description of the snapshot.
--
--
--     * @encrypted@ - Indicates whether the snapshot is encrypted (@true@ | @false@ )
--
--
--     * @owner-alias@ - The owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console. We recommend that you use the related parameter instead of this filter.
--
--
--     * @owner-id@ - The AWS account ID of the owner. We recommend that you use the related parameter instead of this filter.
--
--
--     * @progress@ - The progress of the snapshot, as a percentage (for example, 80%).
--
--
--     * @snapshot-id@ - The snapshot ID.
--
--
--     * @start-time@ - The time stamp when the snapshot was initiated.
--
--
--     * @status@ - The status of the snapshot (@pending@ | @completed@ | @error@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @volume-id@ - The ID of the volume the snapshot is for.
--
--
--     * @volume-size@ - The size of the volume, in GiB.
--
--
-- * 'nextToken' - The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
-- * 'snapshotIds' - The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume permissions.
-- * 'restorableByUserIds' - The IDs of the AWS accounts that can create volumes from the snapshot.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
mkDescribeSnapshots ::
  DescribeSnapshots
mkDescribeSnapshots =
  DescribeSnapshots'
    { ownerIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      snapshotIds = Lude.Nothing,
      restorableByUserIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
--
-- /Note:/ Consider using 'ownerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOwnerIds :: Lens.Lens' DescribeSnapshots (Lude.Maybe [Lude.Text])
dssOwnerIds = Lens.lens (ownerIds :: DescribeSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {ownerIds = a} :: DescribeSnapshots)
{-# DEPRECATED dssOwnerIds "Use generic-lens or generic-optics with 'ownerIds' instead." #-}

-- | The filters.
--
--
--     * @description@ - A description of the snapshot.
--
--
--     * @encrypted@ - Indicates whether the snapshot is encrypted (@true@ | @false@ )
--
--
--     * @owner-alias@ - The owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console. We recommend that you use the related parameter instead of this filter.
--
--
--     * @owner-id@ - The AWS account ID of the owner. We recommend that you use the related parameter instead of this filter.
--
--
--     * @progress@ - The progress of the snapshot, as a percentage (for example, 80%).
--
--
--     * @snapshot-id@ - The snapshot ID.
--
--
--     * @start-time@ - The time stamp when the snapshot was initiated.
--
--
--     * @status@ - The status of the snapshot (@pending@ | @completed@ | @error@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @volume-id@ - The ID of the volume the snapshot is for.
--
--
--     * @volume-size@ - The size of the volume, in GiB.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssFilters :: Lens.Lens' DescribeSnapshots (Lude.Maybe [Filter])
dssFilters = Lens.lens (filters :: DescribeSnapshots -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSnapshots)
{-# DEPRECATED dssFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dssNextToken = Lens.lens (nextToken :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSnapshots)
{-# DEPRECATED dssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume permissions.
--
-- /Note:/ Consider using 'snapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssSnapshotIds :: Lens.Lens' DescribeSnapshots (Lude.Maybe [Lude.Text])
dssSnapshotIds = Lens.lens (snapshotIds :: DescribeSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {snapshotIds = a} :: DescribeSnapshots)
{-# DEPRECATED dssSnapshotIds "Use generic-lens or generic-optics with 'snapshotIds' instead." #-}

-- | The IDs of the AWS accounts that can create volumes from the snapshot.
--
-- /Note:/ Consider using 'restorableByUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssRestorableByUserIds :: Lens.Lens' DescribeSnapshots (Lude.Maybe [Lude.Text])
dssRestorableByUserIds = Lens.lens (restorableByUserIds :: DescribeSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {restorableByUserIds = a} :: DescribeSnapshots)
{-# DEPRECATED dssRestorableByUserIds "Use generic-lens or generic-optics with 'restorableByUserIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDryRun :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Bool)
dssDryRun = Lens.lens (dryRun :: DescribeSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSnapshots)
{-# DEPRECATED dssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxResults :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Int)
dssMaxResults = Lens.lens (maxResults :: DescribeSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeSnapshots)
{-# DEPRECATED dssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. dssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dssNextToken Lens..~ rs Lens.^. dssrsNextToken

instance Lude.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSnapshotsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "snapshotSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshots where
  toQuery DescribeSnapshots' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Owner" Lude.<$> ownerIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "SnapshotId" Lude.<$> snapshotIds),
        Lude.toQuery
          (Lude.toQueryList "RestorableBy" Lude.<$> restorableByUserIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the snapshots.
    snapshots :: Lude.Maybe [Snapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'snapshots' - Information about the snapshots.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotsResponse
mkDescribeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { nextToken = Lude.Nothing,
      snapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsNextToken :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe Lude.Text)
dssrsNextToken = Lens.lens (nextToken :: DescribeSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe [Snapshot])
dssrsSnapshots = Lens.lens (snapshots :: DescribeSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {snapshots = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
