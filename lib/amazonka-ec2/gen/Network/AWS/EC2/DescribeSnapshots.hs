{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeSnapshots (..)
    , mkDescribeSnapshots
    -- ** Request lenses
    , dssDryRun
    , dssFilters
    , dssMaxResults
    , dssNextToken
    , dssOwnerIds
    , dssRestorableByUserIds
    , dssSnapshotIds

    -- * Destructuring the response
    , DescribeSnapshotsResponse (..)
    , mkDescribeSnapshotsResponse
    -- ** Response lenses
    , dsrfrsNextToken
    , dsrfrsSnapshots
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
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
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
  , ownerIds :: Core.Maybe [Core.Text]
    -- ^ Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
  , restorableByUserIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the AWS accounts that can create volumes from the snapshot.
  , snapshotIds :: Core.Maybe [Types.SnapshotId]
    -- ^ The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume permissions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshots' value with any optional fields omitted.
mkDescribeSnapshots
    :: DescribeSnapshots
mkDescribeSnapshots
  = DescribeSnapshots'{dryRun = Core.Nothing, filters = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing,
                       ownerIds = Core.Nothing, restorableByUserIds = Core.Nothing,
                       snapshotIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDryRun :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Bool)
dssDryRun = Lens.field @"dryRun"
{-# INLINEABLE dssDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dssFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of snapshot results returned by @DescribeSnapshots@ in paginated output. When this parameter is used, @DescribeSnapshots@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeSnapshots@ request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeSnapshots@ returns all results. You cannot specify this parameter and the snapshot IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxResults :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Int)
dssMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dssMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @NextToken@ value returned from a previous paginated @DescribeSnapshots@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dssNextToken = Lens.field @"nextToken"
{-# INLINEABLE dssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Scopes the results to snapshots with the specified owners. You can specify a combination of AWS account IDs, @self@ , and @amazon@ .
--
-- /Note:/ Consider using 'ownerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOwnerIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Core.Text])
dssOwnerIds = Lens.field @"ownerIds"
{-# INLINEABLE dssOwnerIds #-}
{-# DEPRECATED ownerIds "Use generic-lens or generic-optics with 'ownerIds' instead"  #-}

-- | The IDs of the AWS accounts that can create volumes from the snapshot.
--
-- /Note:/ Consider using 'restorableByUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssRestorableByUserIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Core.Text])
dssRestorableByUserIds = Lens.field @"restorableByUserIds"
{-# INLINEABLE dssRestorableByUserIds #-}
{-# DEPRECATED restorableByUserIds "Use generic-lens or generic-optics with 'restorableByUserIds' instead"  #-}

-- | The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume permissions.
--
-- /Note:/ Consider using 'snapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssSnapshotIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.SnapshotId])
dssSnapshotIds = Lens.field @"snapshotIds"
{-# INLINEABLE dssSnapshotIds #-}
{-# DEPRECATED snapshotIds "Use generic-lens or generic-optics with 'snapshotIds' instead"  #-}

instance Core.ToQuery DescribeSnapshots where
        toQuery DescribeSnapshots{..}
          = Core.toQueryPair "Action" ("DescribeSnapshots" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Owner") ownerIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RestorableBy")
                restorableByUserIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SnapshotId") snapshotIds

instance Core.ToHeaders DescribeSnapshots where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeSnapshotsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "snapshotSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , snapshots :: Core.Maybe [Types.Snapshot]
    -- ^ Information about the snapshots.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSnapshotsResponse' value with any optional fields omitted.
mkDescribeSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotsResponse
mkDescribeSnapshotsResponse responseStatus
  = DescribeSnapshotsResponse'{nextToken = Core.Nothing,
                               snapshots = Core.Nothing, responseStatus}

-- | The @NextToken@ value to include in a future @DescribeSnapshots@ request. When the results of a @DescribeSnapshots@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Core.Text)
dsrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Types.Snapshot])
dsrfrsSnapshots = Lens.field @"snapshots"
{-# INLINEABLE dsrfrsSnapshots #-}
{-# DEPRECATED snapshots "Use generic-lens or generic-optics with 'snapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
