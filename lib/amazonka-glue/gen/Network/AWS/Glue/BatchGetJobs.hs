{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of job names. After calling the @ListJobs@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetJobs
  ( -- * Creating a request
    BatchGetJobs (..),
    mkBatchGetJobs,

    -- ** Request lenses
    bgjJobNames,

    -- * Destructuring the response
    BatchGetJobsResponse (..),
    mkBatchGetJobsResponse,

    -- ** Response lenses
    bgjrrsJobs,
    bgjrrsJobsNotFound,
    bgjrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetJobs' smart constructor.
newtype BatchGetJobs = BatchGetJobs'
  { -- | A list of job names, which might be the names returned from the @ListJobs@ operation.
    jobNames :: [Types.NameString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetJobs' value with any optional fields omitted.
mkBatchGetJobs ::
  BatchGetJobs
mkBatchGetJobs = BatchGetJobs' {jobNames = Core.mempty}

-- | A list of job names, which might be the names returned from the @ListJobs@ operation.
--
-- /Note:/ Consider using 'jobNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjJobNames :: Lens.Lens' BatchGetJobs [Types.NameString]
bgjJobNames = Lens.field @"jobNames"
{-# DEPRECATED bgjJobNames "Use generic-lens or generic-optics with 'jobNames' instead." #-}

instance Core.FromJSON BatchGetJobs where
  toJSON BatchGetJobs {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobNames" Core..= jobNames)])

instance Core.AWSRequest BatchGetJobs where
  type Rs BatchGetJobs = BatchGetJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchGetJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            Core.<$> (x Core..:? "Jobs")
            Core.<*> (x Core..:? "JobsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { -- | A list of job definitions.
    jobs :: Core.Maybe [Types.Job],
    -- | A list of names of jobs not found.
    jobsNotFound :: Core.Maybe [Types.NameString],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetJobsResponse' value with any optional fields omitted.
mkBatchGetJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetJobsResponse
mkBatchGetJobsResponse responseStatus =
  BatchGetJobsResponse'
    { jobs = Core.Nothing,
      jobsNotFound = Core.Nothing,
      responseStatus
    }

-- | A list of job definitions.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrrsJobs :: Lens.Lens' BatchGetJobsResponse (Core.Maybe [Types.Job])
bgjrrsJobs = Lens.field @"jobs"
{-# DEPRECATED bgjrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | A list of names of jobs not found.
--
-- /Note:/ Consider using 'jobsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrrsJobsNotFound :: Lens.Lens' BatchGetJobsResponse (Core.Maybe [Types.NameString])
bgjrrsJobsNotFound = Lens.field @"jobsNotFound"
{-# DEPRECATED bgjrrsJobsNotFound "Use generic-lens or generic-optics with 'jobsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrrsResponseStatus :: Lens.Lens' BatchGetJobsResponse Core.Int
bgjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
