{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an events detection job in progress.
module Network.AWS.Comprehend.StopEventsDetectionJob
  ( -- * Creating a request
    StopEventsDetectionJob (..),
    mkStopEventsDetectionJob,

    -- ** Request lenses
    sedjJobId,

    -- * Destructuring the response
    StopEventsDetectionJobResponse (..),
    mkStopEventsDetectionJobResponse,

    -- ** Response lenses
    sedjrrsJobId,
    sedjrrsJobStatus,
    sedjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopEventsDetectionJob' smart constructor.
newtype StopEventsDetectionJob = StopEventsDetectionJob'
  { -- | The identifier of the events detection job to stop.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopEventsDetectionJob' value with any optional fields omitted.
mkStopEventsDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  StopEventsDetectionJob
mkStopEventsDetectionJob jobId = StopEventsDetectionJob' {jobId}

-- | The identifier of the events detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjJobId :: Lens.Lens' StopEventsDetectionJob Types.JobId
sedjJobId = Lens.field @"jobId"
{-# DEPRECATED sedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON StopEventsDetectionJob where
  toJSON StopEventsDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopEventsDetectionJob where
  type Rs StopEventsDetectionJob = StopEventsDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.StopEventsDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEventsDetectionJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopEventsDetectionJobResponse' smart constructor.
data StopEventsDetectionJobResponse = StopEventsDetectionJobResponse'
  { -- | The identifier of the events detection job to stop.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the events detection job.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopEventsDetectionJobResponse' value with any optional fields omitted.
mkStopEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopEventsDetectionJobResponse
mkStopEventsDetectionJobResponse responseStatus =
  StopEventsDetectionJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier of the events detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrrsJobId :: Lens.Lens' StopEventsDetectionJobResponse (Core.Maybe Types.JobId)
sedjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED sedjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrrsJobStatus :: Lens.Lens' StopEventsDetectionJobResponse (Core.Maybe Types.JobStatus)
sedjrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED sedjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrrsResponseStatus :: Lens.Lens' StopEventsDetectionJobResponse Core.Int
sedjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sedjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
