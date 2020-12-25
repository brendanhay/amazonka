{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of AWS Batch jobs.
module Network.AWS.Batch.DescribeJobs
  ( -- * Creating a request
    DescribeJobs (..),
    mkDescribeJobs,

    -- ** Request lenses
    djJobs,

    -- * Destructuring the response
    DescribeJobsResponse (..),
    mkDescribeJobsResponse,

    -- ** Response lenses
    djrrsJobs,
    djrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeJobs' smart constructor.
newtype DescribeJobs = DescribeJobs'
  { -- | A list of up to 100 job IDs.
    jobs :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobs' value with any optional fields omitted.
mkDescribeJobs ::
  DescribeJobs
mkDescribeJobs = DescribeJobs' {jobs = Core.mempty}

-- | A list of up to 100 job IDs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobs :: Lens.Lens' DescribeJobs [Types.String]
djJobs = Lens.field @"jobs"
{-# DEPRECATED djJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

instance Core.FromJSON DescribeJobs where
  toJSON DescribeJobs {..} =
    Core.object (Core.catMaybes [Core.Just ("jobs" Core..= jobs)])

instance Core.AWSRequest DescribeJobs where
  type Rs DescribeJobs = DescribeJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/describejobs",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobsResponse'
            Core.<$> (x Core..:? "jobs") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeJobsResponse' smart constructor.
data DescribeJobsResponse = DescribeJobsResponse'
  { -- | The list of jobs.
    jobs :: Core.Maybe [Types.JobDetail],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobsResponse' value with any optional fields omitted.
mkDescribeJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeJobsResponse
mkDescribeJobsResponse responseStatus =
  DescribeJobsResponse' {jobs = Core.Nothing, responseStatus}

-- | The list of jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsJobs :: Lens.Lens' DescribeJobsResponse (Core.Maybe [Types.JobDetail])
djrrsJobs = Lens.field @"jobs"
{-# DEPRECATED djrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsResponseStatus :: Lens.Lens' DescribeJobsResponse Core.Int
djrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED djrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
