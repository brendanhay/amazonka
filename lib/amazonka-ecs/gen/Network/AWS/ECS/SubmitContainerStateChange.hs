{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SubmitContainerStateChange (..)
    , mkSubmitContainerStateChange
    -- ** Request lenses
    , scscCluster
    , scscContainerName
    , scscExitCode
    , scscNetworkBindings
    , scscReason
    , scscRuntimeId
    , scscStatus
    , scscTask

    -- * Destructuring the response
    , SubmitContainerStateChangeResponse (..)
    , mkSubmitContainerStateChangeResponse
    -- ** Response lenses
    , scscrrsAcknowledgment
    , scscrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
  { cluster :: Core.Maybe Core.Text
    -- ^ The short name or full ARN of the cluster that hosts the container.
  , containerName :: Core.Maybe Core.Text
    -- ^ The name of the container.
  , exitCode :: Core.Maybe Core.Int
    -- ^ The exit code returned for the state change request.
  , networkBindings :: Core.Maybe [Types.NetworkBinding]
    -- ^ The network bindings of the container.
  , reason :: Core.Maybe Core.Text
    -- ^ The reason for the state change request.
  , runtimeId :: Core.Maybe Core.Text
    -- ^ The ID of the Docker container.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the state change request.
  , task :: Core.Maybe Core.Text
    -- ^ The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitContainerStateChange' value with any optional fields omitted.
mkSubmitContainerStateChange
    :: SubmitContainerStateChange
mkSubmitContainerStateChange
  = SubmitContainerStateChange'{cluster = Core.Nothing,
                                containerName = Core.Nothing, exitCode = Core.Nothing,
                                networkBindings = Core.Nothing, reason = Core.Nothing,
                                runtimeId = Core.Nothing, status = Core.Nothing,
                                task = Core.Nothing}

-- | The short name or full ARN of the cluster that hosts the container.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscCluster :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscCluster = Lens.field @"cluster"
{-# INLINEABLE scscCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscContainerName :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscContainerName = Lens.field @"containerName"
{-# INLINEABLE scscContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The exit code returned for the state change request.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscExitCode :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Int)
scscExitCode = Lens.field @"exitCode"
{-# INLINEABLE scscExitCode #-}
{-# DEPRECATED exitCode "Use generic-lens or generic-optics with 'exitCode' instead"  #-}

-- | The network bindings of the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscNetworkBindings :: Lens.Lens' SubmitContainerStateChange (Core.Maybe [Types.NetworkBinding])
scscNetworkBindings = Lens.field @"networkBindings"
{-# INLINEABLE scscNetworkBindings #-}
{-# DEPRECATED networkBindings "Use generic-lens or generic-optics with 'networkBindings' instead"  #-}

-- | The reason for the state change request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscReason :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscReason = Lens.field @"reason"
{-# INLINEABLE scscReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscRuntimeId :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscRuntimeId = Lens.field @"runtimeId"
{-# INLINEABLE scscRuntimeId #-}
{-# DEPRECATED runtimeId "Use generic-lens or generic-optics with 'runtimeId' instead"  #-}

-- | The status of the state change request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscStatus :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscStatus = Lens.field @"status"
{-# INLINEABLE scscStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscTask :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
scscTask = Lens.field @"task"
{-# INLINEABLE scscTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

instance Core.ToQuery SubmitContainerStateChange where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SubmitContainerStateChange where
        toHeaders SubmitContainerStateChange{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SubmitContainerStateChange where
        toJSON SubmitContainerStateChange{..}
          = Core.object
              (Core.catMaybes
                 [("cluster" Core..=) Core.<$> cluster,
                  ("containerName" Core..=) Core.<$> containerName,
                  ("exitCode" Core..=) Core.<$> exitCode,
                  ("networkBindings" Core..=) Core.<$> networkBindings,
                  ("reason" Core..=) Core.<$> reason,
                  ("runtimeId" Core..=) Core.<$> runtimeId,
                  ("status" Core..=) Core.<$> status,
                  ("task" Core..=) Core.<$> task])

instance Core.AWSRequest SubmitContainerStateChange where
        type Rs SubmitContainerStateChange =
             SubmitContainerStateChangeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SubmitContainerStateChangeResponse' Core.<$>
                   (x Core..:? "acknowledgment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSubmitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
  { acknowledgment :: Core.Maybe Core.Text
    -- ^ Acknowledgement of the state change.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitContainerStateChangeResponse' value with any optional fields omitted.
mkSubmitContainerStateChangeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SubmitContainerStateChangeResponse
mkSubmitContainerStateChangeResponse responseStatus
  = SubmitContainerStateChangeResponse'{acknowledgment =
                                          Core.Nothing,
                                        responseStatus}

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrrsAcknowledgment :: Lens.Lens' SubmitContainerStateChangeResponse (Core.Maybe Core.Text)
scscrrsAcknowledgment = Lens.field @"acknowledgment"
{-# INLINEABLE scscrrsAcknowledgment #-}
{-# DEPRECATED acknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrrsResponseStatus :: Lens.Lens' SubmitContainerStateChangeResponse Core.Int
scscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
