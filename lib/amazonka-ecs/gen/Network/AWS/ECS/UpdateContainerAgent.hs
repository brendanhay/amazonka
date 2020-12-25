{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateContainerAgent (..),
    mkUpdateContainerAgent,

    -- ** Request lenses
    ucaContainerInstance,
    ucaCluster,

    -- * Destructuring the response
    UpdateContainerAgentResponse (..),
    mkUpdateContainerAgentResponse,

    -- ** Response lenses
    ucarrsContainerInstance,
    ucarrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { -- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
    containerInstance :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContainerAgent' value with any optional fields omitted.
mkUpdateContainerAgent ::
  -- | 'containerInstance'
  Types.String ->
  UpdateContainerAgent
mkUpdateContainerAgent containerInstance =
  UpdateContainerAgent' {containerInstance, cluster = Core.Nothing}

-- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaContainerInstance :: Lens.Lens' UpdateContainerAgent Types.String
ucaContainerInstance = Lens.field @"containerInstance"
{-# DEPRECATED ucaContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaCluster :: Lens.Lens' UpdateContainerAgent (Core.Maybe Types.String)
ucaCluster = Lens.field @"cluster"
{-# DEPRECATED ucaCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

instance Core.FromJSON UpdateContainerAgent where
  toJSON UpdateContainerAgent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("containerInstance" Core..= containerInstance),
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.AWSRequest UpdateContainerAgent where
  type Rs UpdateContainerAgent = UpdateContainerAgentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.UpdateContainerAgent"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerAgentResponse'
            Core.<$> (x Core..:? "containerInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { -- | The container instance for which the container agent was updated.
    containerInstance :: Core.Maybe Types.ContainerInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateContainerAgentResponse' value with any optional fields omitted.
mkUpdateContainerAgentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateContainerAgentResponse
mkUpdateContainerAgentResponse responseStatus =
  UpdateContainerAgentResponse'
    { containerInstance = Core.Nothing,
      responseStatus
    }

-- | The container instance for which the container agent was updated.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarrsContainerInstance :: Lens.Lens' UpdateContainerAgentResponse (Core.Maybe Types.ContainerInstance)
ucarrsContainerInstance = Lens.field @"containerInstance"
{-# DEPRECATED ucarrsContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarrsResponseStatus :: Lens.Lens' UpdateContainerAgentResponse Core.Int
ucarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
