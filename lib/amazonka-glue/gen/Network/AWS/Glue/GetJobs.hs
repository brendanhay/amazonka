{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobs
    (
    -- * Creating a request
      GetJobs (..)
    , mkGetJobs
    -- ** Request lenses
    , gjMaxResults
    , gjNextToken

    -- * Destructuring the response
    , GetJobsResponse (..)
    , mkGetJobsResponse
    -- ** Response lenses
    , gjrfrsJobs
    , gjrfrsNextToken
    , gjrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobs' smart constructor.
data GetJobs = GetJobs'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of the response.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobs' value with any optional fields omitted.
mkGetJobs
    :: GetJobs
mkGetJobs
  = GetJobs'{maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjMaxResults :: Lens.Lens' GetJobs (Core.Maybe Core.Natural)
gjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjNextToken :: Lens.Lens' GetJobs (Core.Maybe Types.GenericString)
gjNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJobs where
        toHeaders GetJobs{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetJobs") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetJobs where
        toJSON GetJobs{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetJobs where
        type Rs GetJobs = GetJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobsResponse' Core.<$>
                   (x Core..:? "Jobs") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"jobs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { jobs :: Core.Maybe [Types.Job]
    -- ^ A list of job definitions.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if not all job definitions have yet been returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetJobsResponse' value with any optional fields omitted.
mkGetJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobsResponse
mkGetJobsResponse responseStatus
  = GetJobsResponse'{jobs = Core.Nothing, nextToken = Core.Nothing,
                     responseStatus}

-- | A list of job definitions.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrfrsJobs :: Lens.Lens' GetJobsResponse (Core.Maybe [Types.Job])
gjrfrsJobs = Lens.field @"jobs"
{-# INLINEABLE gjrfrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | A continuation token, if not all job definitions have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrfrsNextToken :: Lens.Lens' GetJobsResponse (Core.Maybe Types.NextToken)
gjrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrfrsResponseStatus :: Lens.Lens' GetJobsResponse Core.Int
gjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
