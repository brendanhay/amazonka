{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task. Any tags associated with the task will be deleted.
--
-- When 'StopTask' is called on a task, the equivalent of @docker stop@ is issued to the containers running in the task. This results in a @SIGTERM@ value and a default 30-second timeout, after which the @SIGKILL@ value is sent and the containers are forcibly stopped. If the container handles the @SIGTERM@ value gracefully and exits within 30 seconds from receiving it, no @SIGKILL@ value is sent.
module Network.AWS.ECS.StopTask
    (
    -- * Creating a request
      StopTask (..)
    , mkStopTask
    -- ** Request lenses
    , stTask
    , stCluster
    , stReason

    -- * Destructuring the response
    , StopTaskResponse (..)
    , mkStopTaskResponse
    -- ** Response lenses
    , srsTask
    , srsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopTask' smart constructor.
data StopTask = StopTask'
  { task :: Core.Text
    -- ^ The task ID or full Amazon Resource Name (ARN) of the task to stop.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
  , reason :: Core.Maybe Core.Text
    -- ^ An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTask' value with any optional fields omitted.
mkStopTask
    :: Core.Text -- ^ 'task'
    -> StopTask
mkStopTask task
  = StopTask'{task, cluster = Core.Nothing, reason = Core.Nothing}

-- | The task ID or full Amazon Resource Name (ARN) of the task to stop.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTask :: Lens.Lens' StopTask Core.Text
stTask = Lens.field @"task"
{-# INLINEABLE stTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCluster :: Lens.Lens' StopTask (Core.Maybe Core.Text)
stCluster = Lens.field @"cluster"
{-# INLINEABLE stCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stReason :: Lens.Lens' StopTask (Core.Maybe Core.Text)
stReason = Lens.field @"reason"
{-# INLINEABLE stReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery StopTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopTask where
        toHeaders StopTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerServiceV20141113.StopTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopTask where
        toJSON StopTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("task" Core..= task),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("reason" Core..=) Core.<$> reason])

instance Core.AWSRequest StopTask where
        type Rs StopTask = StopTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopTaskResponse' Core.<$>
                   (x Core..:? "task") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopTaskResponse' smart constructor.
data StopTaskResponse = StopTaskResponse'
  { task :: Core.Maybe Types.Task
    -- ^ The task that was stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopTaskResponse' value with any optional fields omitted.
mkStopTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopTaskResponse
mkStopTaskResponse responseStatus
  = StopTaskResponse'{task = Core.Nothing, responseStatus}

-- | The task that was stopped.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsTask :: Lens.Lens' StopTaskResponse (Core.Maybe Types.Task)
srsTask = Lens.field @"task"
{-# INLINEABLE srsTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopTaskResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
