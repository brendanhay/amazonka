{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobRuns
    (
    -- * Creating a request
      GetJobRuns (..)
    , mkGetJobRuns
    -- ** Request lenses
    , gjrJobName
    , gjrMaxResults
    , gjrNextToken

    -- * Destructuring the response
    , GetJobRunsResponse (..)
    , mkGetJobRunsResponse
    -- ** Response lenses
    , gjrrrsJobRuns
    , gjrrrsNextToken
    , gjrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { jobName :: Types.JobName
    -- ^ The name of the job definition for which to retrieve all job runs.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of the response.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobRuns' value with any optional fields omitted.
mkGetJobRuns
    :: Types.JobName -- ^ 'jobName'
    -> GetJobRuns
mkGetJobRuns jobName
  = GetJobRuns'{jobName, maxResults = Core.Nothing,
                nextToken = Core.Nothing}

-- | The name of the job definition for which to retrieve all job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrJobName :: Lens.Lens' GetJobRuns Types.JobName
gjrJobName = Lens.field @"jobName"
{-# INLINEABLE gjrJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrMaxResults :: Lens.Lens' GetJobRuns (Core.Maybe Core.Natural)
gjrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gjrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrNextToken :: Lens.Lens' GetJobRuns (Core.Maybe Types.NextToken)
gjrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetJobRuns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJobRuns where
        toHeaders GetJobRuns{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetJobRuns") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetJobRuns where
        toJSON GetJobRuns{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobName" Core..= jobName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetJobRuns where
        type Rs GetJobRuns = GetJobRunsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobRunsResponse' Core.<$>
                   (x Core..:? "JobRuns") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetJobRuns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"jobRuns" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { jobRuns :: Core.Maybe [Types.JobRun]
    -- ^ A list of job-run metadata objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if not all requested job runs have been returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetJobRunsResponse' value with any optional fields omitted.
mkGetJobRunsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobRunsResponse
mkGetJobRunsResponse responseStatus
  = GetJobRunsResponse'{jobRuns = Core.Nothing,
                        nextToken = Core.Nothing, responseStatus}

-- | A list of job-run metadata objects.
--
-- /Note:/ Consider using 'jobRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsJobRuns :: Lens.Lens' GetJobRunsResponse (Core.Maybe [Types.JobRun])
gjrrrsJobRuns = Lens.field @"jobRuns"
{-# INLINEABLE gjrrrsJobRuns #-}
{-# DEPRECATED jobRuns "Use generic-lens or generic-optics with 'jobRuns' instead"  #-}

-- | A continuation token, if not all requested job runs have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsNextToken :: Lens.Lens' GetJobRunsResponse (Core.Maybe Types.NextToken)
gjrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gjrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsResponseStatus :: Lens.Lens' GetJobRunsResponse Core.Int
gjrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
