{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceDeployment
  ( ContainerServiceDeployment (..)
  -- * Smart constructor
  , mkContainerServiceDeployment
  -- * Lenses
  , csdContainers
  , csdCreatedAt
  , csdPublicEndpoint
  , csdState
  , csdVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Container as Types
import qualified Network.AWS.Lightsail.Types.ContainerName as Types
import qualified Network.AWS.Lightsail.Types.ContainerServiceDeploymentState as Types
import qualified Network.AWS.Lightsail.Types.ContainerServiceEndpoint as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
-- /See:/ 'mkContainerServiceDeployment' smart constructor.
data ContainerServiceDeployment = ContainerServiceDeployment'
  { containers :: Core.Maybe (Core.HashMap Types.ContainerName Types.Container)
    -- ^ An object that describes the configuration for the containers of the deployment.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the deployment was created.
  , publicEndpoint :: Core.Maybe Types.ContainerServiceEndpoint
    -- ^ An object that describes the endpoint of the deployment.
  , state :: Core.Maybe Types.ContainerServiceDeploymentState
    -- ^ The state of the deployment.
--
-- A deployment can be in one of the following states:
--
--     * @Activating@ - The deployment is being created.
--
--
--     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.
--
--
--     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.
--
--
--     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
--
--
  , version :: Core.Maybe Core.Int
    -- ^ The version number of the deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ContainerServiceDeployment' value with any optional fields omitted.
mkContainerServiceDeployment
    :: ContainerServiceDeployment
mkContainerServiceDeployment
  = ContainerServiceDeployment'{containers = Core.Nothing,
                                createdAt = Core.Nothing, publicEndpoint = Core.Nothing,
                                state = Core.Nothing, version = Core.Nothing}

-- | An object that describes the configuration for the containers of the deployment.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdContainers :: Lens.Lens' ContainerServiceDeployment (Core.Maybe (Core.HashMap Types.ContainerName Types.Container))
csdContainers = Lens.field @"containers"
{-# INLINEABLE csdContainers #-}
{-# DEPRECATED containers "Use generic-lens or generic-optics with 'containers' instead"  #-}

-- | The timestamp when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdCreatedAt :: Lens.Lens' ContainerServiceDeployment (Core.Maybe Core.NominalDiffTime)
csdCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE csdCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An object that describes the endpoint of the deployment.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdPublicEndpoint :: Lens.Lens' ContainerServiceDeployment (Core.Maybe Types.ContainerServiceEndpoint)
csdPublicEndpoint = Lens.field @"publicEndpoint"
{-# INLINEABLE csdPublicEndpoint #-}
{-# DEPRECATED publicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead"  #-}

-- | The state of the deployment.
--
-- A deployment can be in one of the following states:
--
--     * @Activating@ - The deployment is being created.
--
--
--     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.
--
--
--     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.
--
--
--     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdState :: Lens.Lens' ContainerServiceDeployment (Core.Maybe Types.ContainerServiceDeploymentState)
csdState = Lens.field @"state"
{-# INLINEABLE csdState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The version number of the deployment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdVersion :: Lens.Lens' ContainerServiceDeployment (Core.Maybe Core.Int)
csdVersion = Lens.field @"version"
{-# INLINEABLE csdVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ContainerServiceDeployment where
        parseJSON
          = Core.withObject "ContainerServiceDeployment" Core.$
              \ x ->
                ContainerServiceDeployment' Core.<$>
                  (x Core..:? "containers") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "publicEndpoint"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "version"
