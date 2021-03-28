{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceEndpoint
  ( ContainerServiceEndpoint (..)
  -- * Smart constructor
  , mkContainerServiceEndpoint
  -- * Lenses
  , cseContainerName
  , cseContainerPort
  , cseHealthCheck
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the public endpoint configuration of a deployment of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceEndpoint' smart constructor.
data ContainerServiceEndpoint = ContainerServiceEndpoint'
  { containerName :: Core.Maybe Core.Text
    -- ^ The name of the container entry of the deployment that the endpoint configuration applies to.
  , containerPort :: Core.Maybe Core.Int
    -- ^ The port of the specified container to which traffic is forwarded to.
  , healthCheck :: Core.Maybe Types.ContainerServiceHealthCheckConfig
    -- ^ An object that describes the health check configuration of the container.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerServiceEndpoint' value with any optional fields omitted.
mkContainerServiceEndpoint
    :: ContainerServiceEndpoint
mkContainerServiceEndpoint
  = ContainerServiceEndpoint'{containerName = Core.Nothing,
                              containerPort = Core.Nothing, healthCheck = Core.Nothing}

-- | The name of the container entry of the deployment that the endpoint configuration applies to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseContainerName :: Lens.Lens' ContainerServiceEndpoint (Core.Maybe Core.Text)
cseContainerName = Lens.field @"containerName"
{-# INLINEABLE cseContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The port of the specified container to which traffic is forwarded to.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseContainerPort :: Lens.Lens' ContainerServiceEndpoint (Core.Maybe Core.Int)
cseContainerPort = Lens.field @"containerPort"
{-# INLINEABLE cseContainerPort #-}
{-# DEPRECATED containerPort "Use generic-lens or generic-optics with 'containerPort' instead"  #-}

-- | An object that describes the health check configuration of the container.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseHealthCheck :: Lens.Lens' ContainerServiceEndpoint (Core.Maybe Types.ContainerServiceHealthCheckConfig)
cseHealthCheck = Lens.field @"healthCheck"
{-# INLINEABLE cseHealthCheck #-}
{-# DEPRECATED healthCheck "Use generic-lens or generic-optics with 'healthCheck' instead"  #-}

instance Core.FromJSON ContainerServiceEndpoint where
        parseJSON
          = Core.withObject "ContainerServiceEndpoint" Core.$
              \ x ->
                ContainerServiceEndpoint' Core.<$>
                  (x Core..:? "containerName") Core.<*> x Core..:? "containerPort"
                    Core.<*> x Core..:? "healthCheck"
