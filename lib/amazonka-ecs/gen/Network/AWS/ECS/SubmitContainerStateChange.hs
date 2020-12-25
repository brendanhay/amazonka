{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitContainerStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that a container changed states.
module Network.AWS.ECS.SubmitContainerStateChange
  ( -- * Creating a request
    SubmitContainerStateChange (..),
    mkSubmitContainerStateChange,

    -- ** Request lenses
    scscCluster,
    scscContainerName,
    scscExitCode,
    scscNetworkBindings,
    scscReason,
    scscRuntimeId,
    scscStatus,
    scscTask,

    -- * Destructuring the response
    SubmitContainerStateChangeResponse (..),
    mkSubmitContainerStateChangeResponse,

    -- ** Response lenses
    scscrrsAcknowledgment,
    scscrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
  { -- | The short name or full ARN of the cluster that hosts the container.
    cluster :: Core.Maybe Types.String,
    -- | The name of the container.
    containerName :: Core.Maybe Types.String,
    -- | The exit code returned for the state change request.
    exitCode :: Core.Maybe Core.Int,
    -- | The network bindings of the container.
    networkBindings :: Core.Maybe [Types.NetworkBinding],
    -- | The reason for the state change request.
    reason :: Core.Maybe Types.String,
    -- | The ID of the Docker container.
    runtimeId :: Core.Maybe Types.String,
    -- | The status of the state change request.
    status :: Core.Maybe Types.String,
    -- | The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
    task :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitContainerStateChange' value with any optional fields omitted.
mkSubmitContainerStateChange ::
  SubmitContainerStateChange
mkSubmitContainerStateChange =
  SubmitContainerStateChange'
    { cluster = Core.Nothing,
      containerName = Core.Nothing,
      exitCode = Core.Nothing,
      networkBindings = Core.Nothing,
      reason = Core.Nothing,
      runtimeId = Core.Nothing,
      status = Core.Nothing,
      task = Core.Nothing
    }

-- | The short name or full ARN of the cluster that hosts the container.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscCluster :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscCluster = Lens.field @"cluster"
{-# DEPRECATED scscCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscContainerName :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscContainerName = Lens.field @"containerName"
{-# DEPRECATED scscContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The exit code returned for the state change request.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscExitCode :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Int)
scscExitCode = Lens.field @"exitCode"
{-# DEPRECATED scscExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The network bindings of the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscNetworkBindings :: Lens.Lens' SubmitContainerStateChange (Core.Maybe [Types.NetworkBinding])
scscNetworkBindings = Lens.field @"networkBindings"
{-# DEPRECATED scscNetworkBindings "Use generic-lens or generic-optics with 'networkBindings' instead." #-}

-- | The reason for the state change request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscReason :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscReason = Lens.field @"reason"
{-# DEPRECATED scscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscRuntimeId :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscRuntimeId = Lens.field @"runtimeId"
{-# DEPRECATED scscRuntimeId "Use generic-lens or generic-optics with 'runtimeId' instead." #-}

-- | The status of the state change request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscStatus :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscStatus = Lens.field @"status"
{-# DEPRECATED scscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscTask :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Types.String)
scscTask = Lens.field @"task"
{-# DEPRECATED scscTask "Use generic-lens or generic-optics with 'task' instead." #-}

instance Core.FromJSON SubmitContainerStateChange where
  toJSON SubmitContainerStateChange {..} =
    Core.object
      ( Core.catMaybes
          [ ("cluster" Core..=) Core.<$> cluster,
            ("containerName" Core..=) Core.<$> containerName,
            ("exitCode" Core..=) Core.<$> exitCode,
            ("networkBindings" Core..=) Core.<$> networkBindings,
            ("reason" Core..=) Core.<$> reason,
            ("runtimeId" Core..=) Core.<$> runtimeId,
            ("status" Core..=) Core.<$> status,
            ("task" Core..=) Core.<$> task
          ]
      )

instance Core.AWSRequest SubmitContainerStateChange where
  type
    Rs SubmitContainerStateChange =
      SubmitContainerStateChangeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitContainerStateChangeResponse'
            Core.<$> (x Core..:? "acknowledgment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSubmitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitContainerStateChangeResponse' value with any optional fields omitted.
mkSubmitContainerStateChangeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SubmitContainerStateChangeResponse
mkSubmitContainerStateChangeResponse responseStatus =
  SubmitContainerStateChangeResponse'
    { acknowledgment =
        Core.Nothing,
      responseStatus
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrrsAcknowledgment :: Lens.Lens' SubmitContainerStateChangeResponse (Core.Maybe Types.String)
scscrrsAcknowledgment = Lens.field @"acknowledgment"
{-# DEPRECATED scscrrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrrsResponseStatus :: Lens.Lens' SubmitContainerStateChangeResponse Core.Int
scscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
