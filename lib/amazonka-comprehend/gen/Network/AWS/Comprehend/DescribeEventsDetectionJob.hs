{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status and details of an events detection job.
module Network.AWS.Comprehend.DescribeEventsDetectionJob
  ( -- * Creating a request
    DescribeEventsDetectionJob (..),
    mkDescribeEventsDetectionJob,

    -- ** Request lenses
    dedjJobId,

    -- * Destructuring the response
    DescribeEventsDetectionJobResponse (..),
    mkDescribeEventsDetectionJobResponse,

    -- ** Response lenses
    dedjrrsEventsDetectionJobProperties,
    dedjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventsDetectionJob' smart constructor.
newtype DescribeEventsDetectionJob = DescribeEventsDetectionJob'
  { -- | The identifier of the events detection job.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventsDetectionJob' value with any optional fields omitted.
mkDescribeEventsDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  DescribeEventsDetectionJob
mkDescribeEventsDetectionJob jobId =
  DescribeEventsDetectionJob' {jobId}

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjJobId :: Lens.Lens' DescribeEventsDetectionJob Types.JobId
dedjJobId = Lens.field @"jobId"
{-# DEPRECATED dedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribeEventsDetectionJob where
  toJSON DescribeEventsDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeEventsDetectionJob where
  type
    Rs DescribeEventsDetectionJob =
      DescribeEventsDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DescribeEventsDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsDetectionJobResponse'
            Core.<$> (x Core..:? "EventsDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { -- | An object that contains the properties associated with an event detection job.
    eventsDetectionJobProperties :: Core.Maybe Types.EventsDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEventsDetectionJobResponse' value with any optional fields omitted.
mkDescribeEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventsDetectionJobResponse
mkDescribeEventsDetectionJobResponse responseStatus =
  DescribeEventsDetectionJobResponse'
    { eventsDetectionJobProperties =
        Core.Nothing,
      responseStatus
    }

-- | An object that contains the properties associated with an event detection job.
--
-- /Note:/ Consider using 'eventsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrrsEventsDetectionJobProperties :: Lens.Lens' DescribeEventsDetectionJobResponse (Core.Maybe Types.EventsDetectionJobProperties)
dedjrrsEventsDetectionJobProperties = Lens.field @"eventsDetectionJobProperties"
{-# DEPRECATED dedjrrsEventsDetectionJobProperties "Use generic-lens or generic-optics with 'eventsDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrrsResponseStatus :: Lens.Lens' DescribeEventsDetectionJobResponse Core.Int
dedjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dedjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
