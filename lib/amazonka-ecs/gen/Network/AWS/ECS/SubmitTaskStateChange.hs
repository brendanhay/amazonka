{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that a task changed states.
module Network.AWS.ECS.SubmitTaskStateChange
  ( -- * Creating a request
    SubmitTaskStateChange (..),
    mkSubmitTaskStateChange,

    -- ** Request lenses
    stscAttachments,
    stscCluster,
    stscContainers,
    stscExecutionStoppedAt,
    stscPullStartedAt,
    stscPullStoppedAt,
    stscReason,
    stscStatus,
    stscTask,

    -- * Destructuring the response
    SubmitTaskStateChangeResponse (..),
    mkSubmitTaskStateChangeResponse,

    -- ** Response lenses
    stscrrsAcknowledgment,
    stscrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
  { -- | Any attachments associated with the state change request.
    attachments :: Core.Maybe [Types.AttachmentStateChange],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task.
    cluster :: Core.Maybe Types.String,
    -- | Any containers associated with the state change request.
    containers :: Core.Maybe [Types.ContainerStateChange],
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The reason for the state change request.
    reason :: Core.Maybe Types.String,
    -- | The status of the state change request.
    status :: Core.Maybe Types.String,
    -- | The task ID or full ARN of the task in the state change request.
    task :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SubmitTaskStateChange' value with any optional fields omitted.
mkSubmitTaskStateChange ::
  SubmitTaskStateChange
mkSubmitTaskStateChange =
  SubmitTaskStateChange'
    { attachments = Core.Nothing,
      cluster = Core.Nothing,
      containers = Core.Nothing,
      executionStoppedAt = Core.Nothing,
      pullStartedAt = Core.Nothing,
      pullStoppedAt = Core.Nothing,
      reason = Core.Nothing,
      status = Core.Nothing,
      task = Core.Nothing
    }

-- | Any attachments associated with the state change request.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscAttachments :: Lens.Lens' SubmitTaskStateChange (Core.Maybe [Types.AttachmentStateChange])
stscAttachments = Lens.field @"attachments"
{-# DEPRECATED stscAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscCluster :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Types.String)
stscCluster = Lens.field @"cluster"
{-# DEPRECATED stscCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Any containers associated with the state change request.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscContainers :: Lens.Lens' SubmitTaskStateChange (Core.Maybe [Types.ContainerStateChange])
stscContainers = Lens.field @"containers"
{-# DEPRECATED stscContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The Unix timestamp for when the task execution stopped.
--
-- /Note:/ Consider using 'executionStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscExecutionStoppedAt :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Core.NominalDiffTime)
stscExecutionStoppedAt = Lens.field @"executionStoppedAt"
{-# DEPRECATED stscExecutionStoppedAt "Use generic-lens or generic-optics with 'executionStoppedAt' instead." #-}

-- | The Unix timestamp for when the container image pull began.
--
-- /Note:/ Consider using 'pullStartedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscPullStartedAt :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Core.NominalDiffTime)
stscPullStartedAt = Lens.field @"pullStartedAt"
{-# DEPRECATED stscPullStartedAt "Use generic-lens or generic-optics with 'pullStartedAt' instead." #-}

-- | The Unix timestamp for when the container image pull completed.
--
-- /Note:/ Consider using 'pullStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscPullStoppedAt :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Core.NominalDiffTime)
stscPullStoppedAt = Lens.field @"pullStoppedAt"
{-# DEPRECATED stscPullStoppedAt "Use generic-lens or generic-optics with 'pullStoppedAt' instead." #-}

-- | The reason for the state change request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscReason :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Types.String)
stscReason = Lens.field @"reason"
{-# DEPRECATED stscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The status of the state change request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscStatus :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Types.String)
stscStatus = Lens.field @"status"
{-# DEPRECATED stscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The task ID or full ARN of the task in the state change request.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscTask :: Lens.Lens' SubmitTaskStateChange (Core.Maybe Types.String)
stscTask = Lens.field @"task"
{-# DEPRECATED stscTask "Use generic-lens or generic-optics with 'task' instead." #-}

instance Core.FromJSON SubmitTaskStateChange where
  toJSON SubmitTaskStateChange {..} =
    Core.object
      ( Core.catMaybes
          [ ("attachments" Core..=) Core.<$> attachments,
            ("cluster" Core..=) Core.<$> cluster,
            ("containers" Core..=) Core.<$> containers,
            ("executionStoppedAt" Core..=) Core.<$> executionStoppedAt,
            ("pullStartedAt" Core..=) Core.<$> pullStartedAt,
            ("pullStoppedAt" Core..=) Core.<$> pullStoppedAt,
            ("reason" Core..=) Core.<$> reason,
            ("status" Core..=) Core.<$> status,
            ("task" Core..=) Core.<$> task
          ]
      )

instance Core.AWSRequest SubmitTaskStateChange where
  type Rs SubmitTaskStateChange = SubmitTaskStateChangeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitTaskStateChangeResponse'
            Core.<$> (x Core..:? "acknowledgment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSubmitTaskStateChangeResponse' smart constructor.
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitTaskStateChangeResponse' value with any optional fields omitted.
mkSubmitTaskStateChangeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SubmitTaskStateChangeResponse
mkSubmitTaskStateChangeResponse responseStatus =
  SubmitTaskStateChangeResponse'
    { acknowledgment = Core.Nothing,
      responseStatus
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscrrsAcknowledgment :: Lens.Lens' SubmitTaskStateChangeResponse (Core.Maybe Types.String)
stscrrsAcknowledgment = Lens.field @"acknowledgment"
{-# DEPRECATED stscrrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscrrsResponseStatus :: Lens.Lens' SubmitTaskStateChangeResponse Core.Int
stscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED stscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
