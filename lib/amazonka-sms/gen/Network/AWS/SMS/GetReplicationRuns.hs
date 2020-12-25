{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetReplicationRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the replication runs for the specified replication job.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationRuns
  ( -- * Creating a request
    GetReplicationRuns (..),
    mkGetReplicationRuns,

    -- ** Request lenses
    grrReplicationJobId,
    grrMaxResults,
    grrNextToken,

    -- * Destructuring the response
    GetReplicationRunsResponse (..),
    mkGetReplicationRunsResponse,

    -- ** Response lenses
    grrrrsNextToken,
    grrrrsReplicationJob,
    grrrrsReplicationRunList,
    grrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { -- | The ID of the replication job.
    replicationJobId :: Types.ReplicationJobId,
    -- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReplicationRuns' value with any optional fields omitted.
mkGetReplicationRuns ::
  -- | 'replicationJobId'
  Types.ReplicationJobId ->
  GetReplicationRuns
mkGetReplicationRuns replicationJobId =
  GetReplicationRuns'
    { replicationJobId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrReplicationJobId :: Lens.Lens' GetReplicationRuns Types.ReplicationJobId
grrReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED grrReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrMaxResults :: Lens.Lens' GetReplicationRuns (Core.Maybe Core.Int)
grrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED grrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextToken :: Lens.Lens' GetReplicationRuns (Core.Maybe Types.NextToken)
grrNextToken = Lens.field @"nextToken"
{-# DEPRECATED grrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetReplicationRuns where
  toJSON GetReplicationRuns {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("replicationJobId" Core..= replicationJobId),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetReplicationRuns where
  type Rs GetReplicationRuns = GetReplicationRunsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GetReplicationRuns"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReplicationRunsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "replicationJob")
            Core.<*> (x Core..:? "replicationRunList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetReplicationRuns where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"replicationRunList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the replication job.
    replicationJob :: Core.Maybe Types.ReplicationJob,
    -- | Information about the replication runs.
    replicationRunList :: Core.Maybe [Types.ReplicationRun],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetReplicationRunsResponse' value with any optional fields omitted.
mkGetReplicationRunsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetReplicationRunsResponse
mkGetReplicationRunsResponse responseStatus =
  GetReplicationRunsResponse'
    { nextToken = Core.Nothing,
      replicationJob = Core.Nothing,
      replicationRunList = Core.Nothing,
      responseStatus
    }

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsNextToken :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe Types.NextToken)
grrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED grrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the replication job.
--
-- /Note:/ Consider using 'replicationJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsReplicationJob :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe Types.ReplicationJob)
grrrrsReplicationJob = Lens.field @"replicationJob"
{-# DEPRECATED grrrrsReplicationJob "Use generic-lens or generic-optics with 'replicationJob' instead." #-}

-- | Information about the replication runs.
--
-- /Note:/ Consider using 'replicationRunList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsReplicationRunList :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe [Types.ReplicationRun])
grrrrsReplicationRunList = Lens.field @"replicationRunList"
{-# DEPRECATED grrrrsReplicationRunList "Use generic-lens or generic-optics with 'replicationRunList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsResponseStatus :: Lens.Lens' GetReplicationRunsResponse Core.Int
grrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
