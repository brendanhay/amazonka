{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.
--
-- If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.
-- Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance. If you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.
module Network.AWS.ECS.DeregisterContainerInstance
    (
    -- * Creating a request
      DeregisterContainerInstance (..)
    , mkDeregisterContainerInstance
    -- ** Request lenses
    , dcifContainerInstance
    , dcifCluster
    , dcifForce

    -- * Destructuring the response
    , DeregisterContainerInstanceResponse (..)
    , mkDeregisterContainerInstanceResponse
    -- ** Response lenses
    , dcirrsContainerInstance
    , dcirrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterContainerInstance' smart constructor.
data DeregisterContainerInstance = DeregisterContainerInstance'
  { containerInstance :: Core.Text
    -- ^ The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
  , force :: Core.Maybe Core.Bool
    -- ^ Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible. 
--
-- Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterContainerInstance' value with any optional fields omitted.
mkDeregisterContainerInstance
    :: Core.Text -- ^ 'containerInstance'
    -> DeregisterContainerInstance
mkDeregisterContainerInstance containerInstance
  = DeregisterContainerInstance'{containerInstance,
                                 cluster = Core.Nothing, force = Core.Nothing}

-- | The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifContainerInstance :: Lens.Lens' DeregisterContainerInstance Core.Text
dcifContainerInstance = Lens.field @"containerInstance"
{-# INLINEABLE dcifContainerInstance #-}
{-# DEPRECATED containerInstance "Use generic-lens or generic-optics with 'containerInstance' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifCluster :: Lens.Lens' DeregisterContainerInstance (Core.Maybe Core.Text)
dcifCluster = Lens.field @"cluster"
{-# INLINEABLE dcifCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible. 
--
-- Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifForce :: Lens.Lens' DeregisterContainerInstance (Core.Maybe Core.Bool)
dcifForce = Lens.field @"force"
{-# INLINEABLE dcifForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery DeregisterContainerInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterContainerInstance where
        toHeaders DeregisterContainerInstance{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DeregisterContainerInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterContainerInstance where
        toJSON DeregisterContainerInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerInstance" Core..= containerInstance),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("force" Core..=) Core.<$> force])

instance Core.AWSRequest DeregisterContainerInstance where
        type Rs DeregisterContainerInstance =
             DeregisterContainerInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeregisterContainerInstanceResponse' Core.<$>
                   (x Core..:? "containerInstance") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterContainerInstanceResponse' smart constructor.
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
  { containerInstance :: Core.Maybe Types.ContainerInstance
    -- ^ The container instance that was deregistered.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeregisterContainerInstanceResponse' value with any optional fields omitted.
mkDeregisterContainerInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterContainerInstanceResponse
mkDeregisterContainerInstanceResponse responseStatus
  = DeregisterContainerInstanceResponse'{containerInstance =
                                           Core.Nothing,
                                         responseStatus}

-- | The container instance that was deregistered.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsContainerInstance :: Lens.Lens' DeregisterContainerInstanceResponse (Core.Maybe Types.ContainerInstance)
dcirrsContainerInstance = Lens.field @"containerInstance"
{-# INLINEABLE dcirrsContainerInstance #-}
{-# DEPRECATED containerInstance "Use generic-lens or generic-optics with 'containerInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsResponseStatus :: Lens.Lens' DeregisterContainerInstanceResponse Core.Int
dcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
