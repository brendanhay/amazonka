{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.
--
-- @UpdateContainerAgent@ requires the Amazon ECS-optimized AMI or Amazon Linux with the @ecs-init@ service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateContainerAgent
    (
    -- * Creating a request
      UpdateContainerAgent (..)
    , mkUpdateContainerAgent
    -- ** Request lenses
    , ucaContainerInstance
    , ucaCluster

    -- * Destructuring the response
    , UpdateContainerAgentResponse (..)
    , mkUpdateContainerAgentResponse
    -- ** Response lenses
    , ucarrsContainerInstance
    , ucarrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { containerInstance :: Core.Text
    -- ^ The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContainerAgent' value with any optional fields omitted.
mkUpdateContainerAgent
    :: Core.Text -- ^ 'containerInstance'
    -> UpdateContainerAgent
mkUpdateContainerAgent containerInstance
  = UpdateContainerAgent'{containerInstance, cluster = Core.Nothing}

-- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaContainerInstance :: Lens.Lens' UpdateContainerAgent Core.Text
ucaContainerInstance = Lens.field @"containerInstance"
{-# INLINEABLE ucaContainerInstance #-}
{-# DEPRECATED containerInstance "Use generic-lens or generic-optics with 'containerInstance' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaCluster :: Lens.Lens' UpdateContainerAgent (Core.Maybe Core.Text)
ucaCluster = Lens.field @"cluster"
{-# INLINEABLE ucaCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery UpdateContainerAgent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateContainerAgent where
        toHeaders UpdateContainerAgent{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateContainerAgent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateContainerAgent where
        toJSON UpdateContainerAgent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerInstance" Core..= containerInstance),
                  ("cluster" Core..=) Core.<$> cluster])

instance Core.AWSRequest UpdateContainerAgent where
        type Rs UpdateContainerAgent = UpdateContainerAgentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateContainerAgentResponse' Core.<$>
                   (x Core..:? "containerInstance") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { containerInstance :: Core.Maybe Types.ContainerInstance
    -- ^ The container instance for which the container agent was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateContainerAgentResponse' value with any optional fields omitted.
mkUpdateContainerAgentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateContainerAgentResponse
mkUpdateContainerAgentResponse responseStatus
  = UpdateContainerAgentResponse'{containerInstance = Core.Nothing,
                                  responseStatus}

-- | The container instance for which the container agent was updated.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarrsContainerInstance :: Lens.Lens' UpdateContainerAgentResponse (Core.Maybe Types.ContainerInstance)
ucarrsContainerInstance = Lens.field @"containerInstance"
{-# INLINEABLE ucarrsContainerInstance #-}
{-# DEPRECATED containerInstance "Use generic-lens or generic-optics with 'containerInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarrsResponseStatus :: Lens.Lens' UpdateContainerAgentResponse Core.Int
ucarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
