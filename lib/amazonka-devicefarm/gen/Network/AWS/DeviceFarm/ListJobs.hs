{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about jobs for a given test run.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListJobs
    (
    -- * Creating a request
      ListJobs (..)
    , mkListJobs
    -- ** Request lenses
    , ljArn
    , ljNextToken

    -- * Destructuring the response
    , ListJobsResponse (..)
    , mkListJobsResponse
    -- ** Response lenses
    , ljrrsJobs
    , ljrrsNextToken
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list jobs operation.
--
-- /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { arn :: Types.Arn
    -- ^ The run's Amazon Resource Name (ARN).
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs
    :: Types.Arn -- ^ 'arn'
    -> ListJobs
mkListJobs arn = ListJobs'{arn, nextToken = Core.Nothing}

-- | The run's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljArn :: Lens.Lens' ListJobs Types.Arn
ljArn = Lens.field @"arn"
{-# INLINEABLE ljArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Core.Maybe Types.PaginationToken)
ljNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListJobs where
        toHeaders ListJobs{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListJobs where
        toJSON ListJobs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobsResponse' Core.<$>
                   (x Core..:? "jobs") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"jobs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the result of a list jobs request.
--
-- /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobs :: Core.Maybe [Types.Job]
    -- ^ Information about the jobs.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobsResponse
mkListJobsResponse responseStatus
  = ListJobsResponse'{jobs = Core.Nothing, nextToken = Core.Nothing,
                      responseStatus}

-- | Information about the jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.Job])
ljrrsJobs = Lens.field @"jobs"
{-# INLINEABLE ljrrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsNextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Types.PaginationToken)
ljrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
