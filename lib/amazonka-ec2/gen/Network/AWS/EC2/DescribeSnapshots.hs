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
    dssDryRun,
    dssFilters,
    dssMaxResults,
    dssNextToken,
    dssOwnerIds,
    dssRestorableByUserIds,
    dssSnapshotIds,

    -- * Destructuring the response
    DescribeSnapshotsResponse (..),
    mkDescribeSnapshotsResponse,

    -- ** Response lenses
    dsrfrsNextToken,
    dsrfrsSnapshots,
    dsrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
    maxResults :: Core.Maybe Core.Int,
    -- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
    ownerIds :: Core.Maybe [Types.String],
    -- | The IDs of the AWS accounts that can create volumes from the snapshot.
    restorableByUserIds :: Core.Maybe [Types.String],
    -- | The snapshot IDs.
    --
    -- Default: Describes the snapshots for which you have create volume permissions.
    snapshotIds :: Core.Maybe [Types.SnapshotId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshots' value with any optional fields omitted.
mkDescribeSnapshots ::
  DescribeSnapshots
mkDescribeSnapshots =
  DescribeSnapshots'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      ownerIds = Core.Nothing,
      restorableByUserIds = Core.Nothing,
      snapshotIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDryRun :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Bool)
dssDryRun = Lens.field @"dryRun"
{-# DEPRECATED dssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
dssFilters :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.Filter])
dssFilters = Lens.field @"filters"
{-# DEPRECATED dssFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxResults :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Int)
dssMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dssNextToken = Lens.field @"nextToken"
{-# DEPRECATED dssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
--
-- /Note:/ Consider using 'ownerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOwnerIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.String])
dssOwnerIds = Lens.field @"ownerIds"
{-# DEPRECATED dssOwnerIds "Use generic-lens or generic-optics with 'ownerIds' instead." #-}

-- | The IDs of the AWS accounts that can create volumes from the snapshot.
--
-- /Note:/ Consider using 'restorableByUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssRestorableByUserIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.String])
dssRestorableByUserIds = Lens.field @"restorableByUserIds"
{-# DEPRECATED dssRestorableByUserIds "Use generic-lens or generic-optics with 'restorableByUserIds' instead." #-}

-- | The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume permissions.
--
-- /Note:/ Consider using 'snapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssSnapshotIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.SnapshotId])
dssSnapshotIds = Lens.field @"snapshotIds"
{-# DEPRECATED dssSnapshotIds "Use generic-lens or generic-optics with 'snapshotIds' instead." #-}

instance Core.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSnapshots")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "Owner" Core.<$> ownerIds)
                Core.<> (Core.toQueryList "RestorableBy" Core.<$> restorableByUserIds)
                Core.<> (Core.toQueryList "SnapshotId" Core.<$> snapshotIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "snapshotSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the snapshots.
    snapshots :: Core.Maybe [Types.Snapshot],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSnapshotsResponse' value with any optional fields omitted.
mkDescribeSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSnapshotsResponse
mkDescribeSnapshotsResponse responseStatus =
  DescribeSnapshotsResponse'
    { nextToken = Core.Nothing,
      snapshots = Core.Nothing,
      responseStatus
    }

-- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Types.String)
dsrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Types.Snapshot])
dsrfrsSnapshots = Lens.field @"snapshots"
{-# DEPRECATED dsrfrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
