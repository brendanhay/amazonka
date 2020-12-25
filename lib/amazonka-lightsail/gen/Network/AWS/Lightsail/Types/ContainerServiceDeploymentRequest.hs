{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
  ( ContainerServiceDeploymentRequest (..),

    -- * Smart constructor
    mkContainerServiceDeploymentRequest,

    -- * Lenses
    csdrContainers,
    csdrPublicEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Container as Types
import qualified Network.AWS.Lightsail.Types.ContainerName as Types
import qualified Network.AWS.Lightsail.Types.EndpointRequest as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
-- /See:/ 'mkContainerServiceDeploymentRequest' smart constructor.
data ContainerServiceDeploymentRequest = ContainerServiceDeploymentRequest'
  { -- | An object that describes the configuration for the containers of the deployment.
    containers :: Core.Maybe (Core.HashMap Types.ContainerName Types.Container),
    -- | An object that describes the endpoint of the deployment.
    publicEndpoint :: Core.Maybe Types.EndpointRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerServiceDeploymentRequest' value with any optional fields omitted.
mkContainerServiceDeploymentRequest ::
  ContainerServiceDeploymentRequest
mkContainerServiceDeploymentRequest =
  ContainerServiceDeploymentRequest'
    { containers = Core.Nothing,
      publicEndpoint = Core.Nothing
    }

-- | An object that describes the configuration for the containers of the deployment.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrContainers :: Lens.Lens' ContainerServiceDeploymentRequest (Core.Maybe (Core.HashMap Types.ContainerName Types.Container))
csdrContainers = Lens.field @"containers"
{-# DEPRECATED csdrContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | An object that describes the endpoint of the deployment.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrPublicEndpoint :: Lens.Lens' ContainerServiceDeploymentRequest (Core.Maybe Types.EndpointRequest)
csdrPublicEndpoint = Lens.field @"publicEndpoint"
{-# DEPRECATED csdrPublicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead." #-}

instance Core.FromJSON ContainerServiceDeploymentRequest where
  toJSON ContainerServiceDeploymentRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("containers" Core..=) Core.<$> containers,
            ("publicEndpoint" Core..=) Core.<$> publicEndpoint
          ]
      )
