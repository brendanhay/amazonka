{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment for your Amazon Lightsail container service.
--
-- A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
-- You can deploy containers to your container service using container images from a public registry like Docker Hub, or from your local machine. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-container-images Creating container images for your Amazon Lightsail container services> in the /Lightsail Dev Guide/ .
module Network.AWS.Lightsail.CreateContainerServiceDeployment
  ( -- * Creating a request
    CreateContainerServiceDeployment (..),
    mkCreateContainerServiceDeployment,

    -- ** Request lenses
    ccsdServiceName,
    ccsdContainers,
    ccsdPublicEndpoint,

    -- * Destructuring the response
    CreateContainerServiceDeploymentResponse (..),
    mkCreateContainerServiceDeploymentResponse,

    -- ** Response lenses
    ccsdrrsContainerService,
    ccsdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContainerServiceDeployment' smart constructor.
data CreateContainerServiceDeployment = CreateContainerServiceDeployment'
  { -- | The name of the container service for which to create the deployment.
    serviceName :: Types.ContainerServiceName,
    -- | An object that describes the settings of the containers that will be launched on the container service.
    containers :: Core.Maybe (Core.HashMap Types.ContainerName Types.Container),
    -- | An object that describes the settings of the public endpoint for the container service.
    publicEndpoint :: Core.Maybe Types.EndpointRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContainerServiceDeployment' value with any optional fields omitted.
mkCreateContainerServiceDeployment ::
  -- | 'serviceName'
  Types.ContainerServiceName ->
  CreateContainerServiceDeployment
mkCreateContainerServiceDeployment serviceName =
  CreateContainerServiceDeployment'
    { serviceName,
      containers = Core.Nothing,
      publicEndpoint = Core.Nothing
    }

-- | The name of the container service for which to create the deployment.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdServiceName :: Lens.Lens' CreateContainerServiceDeployment Types.ContainerServiceName
ccsdServiceName = Lens.field @"serviceName"
{-# DEPRECATED ccsdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | An object that describes the settings of the containers that will be launched on the container service.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdContainers :: Lens.Lens' CreateContainerServiceDeployment (Core.Maybe (Core.HashMap Types.ContainerName Types.Container))
ccsdContainers = Lens.field @"containers"
{-# DEPRECATED ccsdContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | An object that describes the settings of the public endpoint for the container service.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdPublicEndpoint :: Lens.Lens' CreateContainerServiceDeployment (Core.Maybe Types.EndpointRequest)
ccsdPublicEndpoint = Lens.field @"publicEndpoint"
{-# DEPRECATED ccsdPublicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead." #-}

instance Core.FromJSON CreateContainerServiceDeployment where
  toJSON CreateContainerServiceDeployment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            ("containers" Core..=) Core.<$> containers,
            ("publicEndpoint" Core..=) Core.<$> publicEndpoint
          ]
      )

instance Core.AWSRequest CreateContainerServiceDeployment where
  type
    Rs CreateContainerServiceDeployment =
      CreateContainerServiceDeploymentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.CreateContainerServiceDeployment"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceDeploymentResponse'
            Core.<$> (x Core..:? "containerService")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateContainerServiceDeploymentResponse' smart constructor.
data CreateContainerServiceDeploymentResponse = CreateContainerServiceDeploymentResponse'
  { -- | An object that describes a container service.
    containerService :: Core.Maybe Types.ContainerService,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateContainerServiceDeploymentResponse' value with any optional fields omitted.
mkCreateContainerServiceDeploymentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateContainerServiceDeploymentResponse
mkCreateContainerServiceDeploymentResponse responseStatus =
  CreateContainerServiceDeploymentResponse'
    { containerService =
        Core.Nothing,
      responseStatus
    }

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdrrsContainerService :: Lens.Lens' CreateContainerServiceDeploymentResponse (Core.Maybe Types.ContainerService)
ccsdrrsContainerService = Lens.field @"containerService"
{-# DEPRECATED ccsdrrsContainerService "Use generic-lens or generic-optics with 'containerService' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsdrrsResponseStatus :: Lens.Lens' CreateContainerServiceDeploymentResponse Core.Int
ccsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
