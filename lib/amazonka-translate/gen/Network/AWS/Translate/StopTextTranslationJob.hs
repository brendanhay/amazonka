{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.StopTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an asynchronous batch translation job that is in progress.
--
-- If the job's state is @IN_PROGRESS@ , the job will be marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state. Otherwise, the job is put into the @STOPPED@ state.
-- Asynchronous batch translation jobs are started with the 'StartTextTranslationJob' operation. You can use the 'DescribeTextTranslationJob' or 'ListTextTranslationJobs' operations to get a batch translation job's @JobId@ .
module Network.AWS.Translate.StopTextTranslationJob
  ( -- * Creating a request
    StopTextTranslationJob (..),
    mkStopTextTranslationJob,

    -- ** Request lenses
    sttjJobId,

    -- * Destructuring the response
    StopTextTranslationJobResponse (..),
    mkStopTextTranslationJobResponse,

    -- ** Response lenses
    sttjrrsJobId,
    sttjrrsJobStatus,
    sttjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkStopTextTranslationJob' smart constructor.
newtype StopTextTranslationJob = StopTextTranslationJob'
  { -- | The job ID of the job to be stopped.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTextTranslationJob' value with any optional fields omitted.
mkStopTextTranslationJob ::
  -- | 'jobId'
  Types.JobId ->
  StopTextTranslationJob
mkStopTextTranslationJob jobId = StopTextTranslationJob' {jobId}

-- | The job ID of the job to be stopped.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjJobId :: Lens.Lens' StopTextTranslationJob Types.JobId
sttjJobId = Lens.field @"jobId"
{-# DEPRECATED sttjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON StopTextTranslationJob where
  toJSON StopTextTranslationJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopTextTranslationJob where
  type Rs StopTextTranslationJob = StopTextTranslationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.StopTextTranslationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTextTranslationJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopTextTranslationJobResponse' smart constructor.
data StopTextTranslationJobResponse = StopTextTranslationJobResponse'
  { -- | The job ID of the stopped batch translation job.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTextTranslationJobResponse' value with any optional fields omitted.
mkStopTextTranslationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopTextTranslationJobResponse
mkStopTextTranslationJobResponse responseStatus =
  StopTextTranslationJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The job ID of the stopped batch translation job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsJobId :: Lens.Lens' StopTextTranslationJobResponse (Core.Maybe Types.JobId)
sttjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED sttjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsJobStatus :: Lens.Lens' StopTextTranslationJobResponse (Core.Maybe Types.JobStatus)
sttjrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED sttjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsResponseStatus :: Lens.Lens' StopTextTranslationJobResponse Core.Int
sttjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sttjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
