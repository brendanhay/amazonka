{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerInstancesState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the status of an Amazon ECS container instance.
--
-- Once a container instance has reached an @ACTIVE@ state, you can change the status of a container instance to @DRAINING@ to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size.
-- /Important:/ A container instance cannot be changed to @DRAINING@ until it has reached an @ACTIVE@ status. If the instance is in any other status, an error will be received.
-- When you set a container instance to @DRAINING@ , Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the @PENDING@ state are stopped immediately.
-- Service tasks on the container instance that are in the @RUNNING@ state are stopped and replaced according to the service's deployment configuration parameters, @minimumHealthyPercent@ and @maximumPercent@ . You can change the deployment configuration of your service using 'UpdateService' .
--
--     * If @minimumHealthyPercent@ is below 100%, the scheduler can ignore @desiredCount@ temporarily during task replacement. For example, @desiredCount@ is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer.
--
--
--     * The @maximumPercent@ parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if @desiredCount@ is four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained, provided that the cluster resources required to do this are available. If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.
--
--
-- Any @PENDING@ or @RUNNING@ tasks that do not belong to a service are not affected. You must wait for them to finish or stop them manually.
-- A container instance has completed draining when it has no more @RUNNING@ tasks. You can verify this using 'ListTasks' .
-- When a container instance has been drained, you can set a container instance to @ACTIVE@ status and once it has reached that status the Amazon ECS scheduler can begin scheduling tasks on the instance again.
module Network.AWS.ECS.UpdateContainerInstancesState
    (
    -- * Creating a request
      UpdateContainerInstancesState (..)
    , mkUpdateContainerInstancesState
    -- ** Request lenses
    , ucisContainerInstances
    , ucisStatus
    , ucisCluster

    -- * Destructuring the response
    , UpdateContainerInstancesStateResponse (..)
    , mkUpdateContainerInstancesStateResponse
    -- ** Response lenses
    , ucisrrsContainerInstances
    , ucisrrsFailures
    , ucisrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContainerInstancesState' smart constructor.
data UpdateContainerInstancesState = UpdateContainerInstancesState'
  { containerInstances :: [Core.Text]
    -- ^ A list of container instance IDs or full ARN entries.
  , status :: Types.ContainerInstanceStatus
    -- ^ The container instance state with which to update the container instance. The only valid values for this action are @ACTIVE@ and @DRAINING@ . A container instance can only be updated to @DRAINING@ status once it has reached an @ACTIVE@ state. If a container instance is in @REGISTERING@ , @DEREGISTERING@ , or @REGISTRATION_FAILED@ state you can describe the container instance but will be unable to update the container instance state.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContainerInstancesState' value with any optional fields omitted.
mkUpdateContainerInstancesState
    :: Types.ContainerInstanceStatus -- ^ 'status'
    -> UpdateContainerInstancesState
mkUpdateContainerInstancesState status
  = UpdateContainerInstancesState'{containerInstances = Core.mempty,
                                   status, cluster = Core.Nothing}

-- | A list of container instance IDs or full ARN entries.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisContainerInstances :: Lens.Lens' UpdateContainerInstancesState [Core.Text]
ucisContainerInstances = Lens.field @"containerInstances"
{-# INLINEABLE ucisContainerInstances #-}
{-# DEPRECATED containerInstances "Use generic-lens or generic-optics with 'containerInstances' instead"  #-}

-- | The container instance state with which to update the container instance. The only valid values for this action are @ACTIVE@ and @DRAINING@ . A container instance can only be updated to @DRAINING@ status once it has reached an @ACTIVE@ state. If a container instance is in @REGISTERING@ , @DEREGISTERING@ , or @REGISTRATION_FAILED@ state you can describe the container instance but will be unable to update the container instance state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisStatus :: Lens.Lens' UpdateContainerInstancesState Types.ContainerInstanceStatus
ucisStatus = Lens.field @"status"
{-# INLINEABLE ucisStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisCluster :: Lens.Lens' UpdateContainerInstancesState (Core.Maybe Core.Text)
ucisCluster = Lens.field @"cluster"
{-# INLINEABLE ucisCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery UpdateContainerInstancesState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateContainerInstancesState where
        toHeaders UpdateContainerInstancesState{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateContainerInstancesState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateContainerInstancesState where
        toJSON UpdateContainerInstancesState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerInstances" Core..= containerInstances),
                  Core.Just ("status" Core..= status),
                  ("cluster" Core..=) Core.<$> cluster])

instance Core.AWSRequest UpdateContainerInstancesState where
        type Rs UpdateContainerInstancesState =
             UpdateContainerInstancesStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateContainerInstancesStateResponse' Core.<$>
                   (x Core..:? "containerInstances") Core.<*> x Core..:? "failures"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateContainerInstancesStateResponse' smart constructor.
data UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse'
  { containerInstances :: Core.Maybe [Types.ContainerInstance]
    -- ^ The list of container instances.
  , failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateContainerInstancesStateResponse' value with any optional fields omitted.
mkUpdateContainerInstancesStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateContainerInstancesStateResponse
mkUpdateContainerInstancesStateResponse responseStatus
  = UpdateContainerInstancesStateResponse'{containerInstances =
                                             Core.Nothing,
                                           failures = Core.Nothing, responseStatus}

-- | The list of container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrrsContainerInstances :: Lens.Lens' UpdateContainerInstancesStateResponse (Core.Maybe [Types.ContainerInstance])
ucisrrsContainerInstances = Lens.field @"containerInstances"
{-# INLINEABLE ucisrrsContainerInstances #-}
{-# DEPRECATED containerInstances "Use generic-lens or generic-optics with 'containerInstances' instead"  #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrrsFailures :: Lens.Lens' UpdateContainerInstancesStateResponse (Core.Maybe [Types.Failure])
ucisrrsFailures = Lens.field @"failures"
{-# INLINEABLE ucisrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrrsResponseStatus :: Lens.Lens' UpdateContainerInstancesStateResponse Core.Int
ucisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
