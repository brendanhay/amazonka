{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeTopicsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeTopicsDetectionJob
  ( -- * Creating a request
    DescribeTopicsDetectionJob (..),
    mkDescribeTopicsDetectionJob,

    -- ** Request lenses
    dtdjJobId,

    -- * Destructuring the response
    DescribeTopicsDetectionJobResponse (..),
    mkDescribeTopicsDetectionJobResponse,

    -- ** Response lenses
    dtdjrrsTopicsDetectionJobProperties,
    dtdjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTopicsDetectionJob' smart constructor.
newtype DescribeTopicsDetectionJob = DescribeTopicsDetectionJob'
  { -- | The identifier assigned by the user to the detection job.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTopicsDetectionJob' value with any optional fields omitted.
mkDescribeTopicsDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  DescribeTopicsDetectionJob
mkDescribeTopicsDetectionJob jobId =
  DescribeTopicsDetectionJob' {jobId}

-- | The identifier assigned by the user to the detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjJobId :: Lens.Lens' DescribeTopicsDetectionJob Types.JobId
dtdjJobId = Lens.field @"jobId"
{-# DEPRECATED dtdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribeTopicsDetectionJob where
  toJSON DescribeTopicsDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeTopicsDetectionJob where
  type
    Rs DescribeTopicsDetectionJob =
      DescribeTopicsDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DescribeTopicsDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicsDetectionJobResponse'
            Core.<$> (x Core..:? "TopicsDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTopicsDetectionJobResponse' smart constructor.
data DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse'
  { -- | The list of properties for the requested job.
    topicsDetectionJobProperties :: Core.Maybe Types.TopicsDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTopicsDetectionJobResponse' value with any optional fields omitted.
mkDescribeTopicsDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTopicsDetectionJobResponse
mkDescribeTopicsDetectionJobResponse responseStatus =
  DescribeTopicsDetectionJobResponse'
    { topicsDetectionJobProperties =
        Core.Nothing,
      responseStatus
    }

-- | The list of properties for the requested job.
--
-- /Note:/ Consider using 'topicsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrrsTopicsDetectionJobProperties :: Lens.Lens' DescribeTopicsDetectionJobResponse (Core.Maybe Types.TopicsDetectionJobProperties)
dtdjrrsTopicsDetectionJobProperties = Lens.field @"topicsDetectionJobProperties"
{-# DEPRECATED dtdjrrsTopicsDetectionJobProperties "Use generic-lens or generic-optics with 'topicsDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrrsResponseStatus :: Lens.Lens' DescribeTopicsDetectionJobResponse Core.Int
dtdjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtdjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
