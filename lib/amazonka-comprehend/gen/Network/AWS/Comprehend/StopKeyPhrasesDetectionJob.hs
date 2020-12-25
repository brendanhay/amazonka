{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a key phrases detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
  ( -- * Creating a request
    StopKeyPhrasesDetectionJob (..),
    mkStopKeyPhrasesDetectionJob,

    -- ** Request lenses
    skpdjJobId,

    -- * Destructuring the response
    StopKeyPhrasesDetectionJobResponse (..),
    mkStopKeyPhrasesDetectionJobResponse,

    -- ** Response lenses
    skpdjrrsJobId,
    skpdjrrsJobStatus,
    skpdjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopKeyPhrasesDetectionJob' smart constructor.
newtype StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopKeyPhrasesDetectionJob' value with any optional fields omitted.
mkStopKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  StopKeyPhrasesDetectionJob
mkStopKeyPhrasesDetectionJob jobId =
  StopKeyPhrasesDetectionJob' {jobId}

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjJobId :: Lens.Lens' StopKeyPhrasesDetectionJob Types.JobId
skpdjJobId = Lens.field @"jobId"
{-# DEPRECATED skpdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON StopKeyPhrasesDetectionJob where
  toJSON StopKeyPhrasesDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopKeyPhrasesDetectionJob where
  type
    Rs StopKeyPhrasesDetectionJob =
      StopKeyPhrasesDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.StopKeyPhrasesDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopKeyPhrasesDetectionJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Core.Maybe Types.JobId,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopKeyPhrasesDetectionJobResponse' value with any optional fields omitted.
mkStopKeyPhrasesDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopKeyPhrasesDetectionJobResponse
mkStopKeyPhrasesDetectionJobResponse responseStatus =
  StopKeyPhrasesDetectionJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsJobId :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobId)
skpdjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED skpdjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsJobStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobStatus)
skpdjrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED skpdjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsResponseStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse Core.Int
skpdjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED skpdjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
