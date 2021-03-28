{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetReplicationRuns (..)
    , mkGetReplicationRuns
    -- ** Request lenses
    , grrReplicationJobId
    , grrMaxResults
    , grrNextToken

    -- * Destructuring the response
    , GetReplicationRunsResponse (..)
    , mkGetReplicationRunsResponse
    -- ** Response lenses
    , grrrrsNextToken
    , grrrrsReplicationJob
    , grrrrsReplicationRunList
    , grrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { replicationJobId :: Types.ReplicationJobId
    -- ^ The ID of the replication job.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReplicationRuns' value with any optional fields omitted.
mkGetReplicationRuns
    :: Types.ReplicationJobId -- ^ 'replicationJobId'
    -> GetReplicationRuns
mkGetReplicationRuns replicationJobId
  = GetReplicationRuns'{replicationJobId, maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrReplicationJobId :: Lens.Lens' GetReplicationRuns Types.ReplicationJobId
grrReplicationJobId = Lens.field @"replicationJobId"
{-# INLINEABLE grrReplicationJobId #-}
{-# DEPRECATED replicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead"  #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrMaxResults :: Lens.Lens' GetReplicationRuns (Core.Maybe Core.Int)
grrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE grrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextToken :: Lens.Lens' GetReplicationRuns (Core.Maybe Types.NextToken)
grrNextToken = Lens.field @"nextToken"
{-# INLINEABLE grrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetReplicationRuns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetReplicationRuns where
        toHeaders GetReplicationRuns{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.GetReplicationRuns")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetReplicationRuns where
        toJSON GetReplicationRuns{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("replicationJobId" Core..= replicationJobId),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetReplicationRuns where
        type Rs GetReplicationRuns = GetReplicationRunsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetReplicationRunsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "replicationJob"
                     Core.<*> x Core..:? "replicationRunList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetReplicationRuns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"replicationRunList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token required to retrieve the next set of results. This value is null when there are no more results to return.
  , replicationJob :: Core.Maybe Types.ReplicationJob
    -- ^ Information about the replication job.
  , replicationRunList :: Core.Maybe [Types.ReplicationRun]
    -- ^ Information about the replication runs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetReplicationRunsResponse' value with any optional fields omitted.
mkGetReplicationRunsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetReplicationRunsResponse
mkGetReplicationRunsResponse responseStatus
  = GetReplicationRunsResponse'{nextToken = Core.Nothing,
                                replicationJob = Core.Nothing, replicationRunList = Core.Nothing,
                                responseStatus}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsNextToken :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe Types.NextToken)
grrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE grrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the replication job.
--
-- /Note:/ Consider using 'replicationJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsReplicationJob :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe Types.ReplicationJob)
grrrrsReplicationJob = Lens.field @"replicationJob"
{-# INLINEABLE grrrrsReplicationJob #-}
{-# DEPRECATED replicationJob "Use generic-lens or generic-optics with 'replicationJob' instead"  #-}

-- | Information about the replication runs.
--
-- /Note:/ Consider using 'replicationRunList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsReplicationRunList :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe [Types.ReplicationRun])
grrrrsReplicationRunList = Lens.field @"replicationRunList"
{-# INLINEABLE grrrrsReplicationRunList #-}
{-# DEPRECATED replicationRunList "Use generic-lens or generic-optics with 'replicationRunList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsResponseStatus :: Lens.Lens' GetReplicationRunsResponse Core.Int
grrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
