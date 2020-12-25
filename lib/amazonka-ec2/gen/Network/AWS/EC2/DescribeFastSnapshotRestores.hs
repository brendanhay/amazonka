{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of fast snapshot restores for your snapshots.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFastSnapshotRestores
  ( -- * Creating a request
    DescribeFastSnapshotRestores (..),
    mkDescribeFastSnapshotRestores,

    -- ** Request lenses
    dfsrDryRun,
    dfsrFilters,
    dfsrMaxResults,
    dfsrNextToken,

    -- * Destructuring the response
    DescribeFastSnapshotRestoresResponse (..),
    mkDescribeFastSnapshotRestoresResponse,

    -- ** Response lenses
    dfsrrrsFastSnapshotRestores,
    dfsrrrsNextToken,
    dfsrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFastSnapshotRestores' smart constructor.
data DescribeFastSnapshotRestores = DescribeFastSnapshotRestores'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters. The possible values are:
    --
    --
    --     * @availability-zone@ : The Availability Zone of the snapshot.
    --
    --
    --     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.
    --
    --
    --     * @snapshot-id@ : The ID of the snapshot.
    --
    --
    --     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFastSnapshotRestores' value with any optional fields omitted.
mkDescribeFastSnapshotRestores ::
  DescribeFastSnapshotRestores
mkDescribeFastSnapshotRestores =
  DescribeFastSnapshotRestores'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrDryRun :: Lens.Lens' DescribeFastSnapshotRestores (Core.Maybe Core.Bool)
dfsrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dfsrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters. The possible values are:
--
--
--     * @availability-zone@ : The Availability Zone of the snapshot.
--
--
--     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.
--
--
--     * @snapshot-id@ : The ID of the snapshot.
--
--
--     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrFilters :: Lens.Lens' DescribeFastSnapshotRestores (Core.Maybe [Types.Filter])
dfsrFilters = Lens.field @"filters"
{-# DEPRECATED dfsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrMaxResults :: Lens.Lens' DescribeFastSnapshotRestores (Core.Maybe Core.Natural)
dfsrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dfsrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrNextToken :: Lens.Lens' DescribeFastSnapshotRestores (Core.Maybe Types.NextToken)
dfsrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfsrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeFastSnapshotRestores where
  type
    Rs DescribeFastSnapshotRestores =
      DescribeFastSnapshotRestoresResponse
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
            ( Core.pure ("Action", "DescribeFastSnapshotRestores")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFastSnapshotRestoresResponse'
            Core.<$> ( x Core..@? "fastSnapshotRestoreSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeFastSnapshotRestores where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"fastSnapshotRestores" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeFastSnapshotRestoresResponse' smart constructor.
data DescribeFastSnapshotRestoresResponse = DescribeFastSnapshotRestoresResponse'
  { -- | Information about the state of fast snapshot restores.
    fastSnapshotRestores :: Core.Maybe [Types.DescribeFastSnapshotRestoreSuccessItem],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeFastSnapshotRestoresResponse' value with any optional fields omitted.
mkDescribeFastSnapshotRestoresResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFastSnapshotRestoresResponse
mkDescribeFastSnapshotRestoresResponse responseStatus =
  DescribeFastSnapshotRestoresResponse'
    { fastSnapshotRestores =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the state of fast snapshot restores.
--
-- /Note:/ Consider using 'fastSnapshotRestores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrrsFastSnapshotRestores :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Core.Maybe [Types.DescribeFastSnapshotRestoreSuccessItem])
dfsrrrsFastSnapshotRestores = Lens.field @"fastSnapshotRestores"
{-# DEPRECATED dfsrrrsFastSnapshotRestores "Use generic-lens or generic-optics with 'fastSnapshotRestores' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrrsNextToken :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Core.Maybe Types.NextToken)
dfsrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfsrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrrsResponseStatus :: Lens.Lens' DescribeFastSnapshotRestoresResponse Core.Int
dfsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
