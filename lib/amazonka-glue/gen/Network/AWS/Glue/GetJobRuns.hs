{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetJobRuns (..),
    mkGetJobRuns,

    -- ** Request lenses
    gjrJobName,
    gjrMaxResults,
    gjrNextToken,

    -- * Destructuring the response
    GetJobRunsResponse (..),
    mkGetJobRunsResponse,

    -- ** Response lenses
    gjrrrsJobRuns,
    gjrrrsNextToken,
    gjrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { -- | The name of the job definition for which to retrieve all job runs.
    jobName :: Types.JobName,
    -- | The maximum size of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobRuns' value with any optional fields omitted.
mkGetJobRuns ::
  -- | 'jobName'
  Types.JobName ->
  GetJobRuns
mkGetJobRuns jobName =
  GetJobRuns'
    { jobName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the job definition for which to retrieve all job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrJobName :: Lens.Lens' GetJobRuns Types.JobName
gjrJobName = Lens.field @"jobName"
{-# DEPRECATED gjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrMaxResults :: Lens.Lens' GetJobRuns (Core.Maybe Core.Natural)
gjrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gjrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrNextToken :: Lens.Lens' GetJobRuns (Core.Maybe Types.NextToken)
gjrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gjrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetJobRuns where
  toJSON GetJobRuns {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetJobRuns where
  type Rs GetJobRuns = GetJobRunsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetJobRuns")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Core.<$> (x Core..:? "JobRuns")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetJobRuns where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"jobRuns" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { -- | A list of job-run metadata objects.
    jobRuns :: Core.Maybe [Types.JobRun],
    -- | A continuation token, if not all requested job runs have been returned.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetJobRunsResponse' value with any optional fields omitted.
mkGetJobRunsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobRunsResponse
mkGetJobRunsResponse responseStatus =
  GetJobRunsResponse'
    { jobRuns = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of job-run metadata objects.
--
-- /Note:/ Consider using 'jobRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsJobRuns :: Lens.Lens' GetJobRunsResponse (Core.Maybe [Types.JobRun])
gjrrrsJobRuns = Lens.field @"jobRuns"
{-# DEPRECATED gjrrrsJobRuns "Use generic-lens or generic-optics with 'jobRuns' instead." #-}

-- | A continuation token, if not all requested job runs have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsNextToken :: Lens.Lens' GetJobRunsResponse (Core.Maybe Types.NextToken)
gjrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gjrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrrsResponseStatus :: Lens.Lens' GetJobRunsResponse Core.Int
gjrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
