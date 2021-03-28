{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.EndpointRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.EndpointRequest
  ( EndpointRequest (..)
  -- * Smart constructor
  , mkEndpointRequest
  -- * Lenses
  , erContainerName
  , erContainerPort
  , erHealthCheck
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the settings of a public endpoint for an Amazon Lightsail container service.
--
-- /See:/ 'mkEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { containerName :: Core.Text
    -- ^ The name of the container for the endpoint.
  , containerPort :: Core.Int
    -- ^ The port of the container to which traffic is forwarded to.
  , healthCheck :: Core.Maybe Types.ContainerServiceHealthCheckConfig
    -- ^ An object that describes the health check configuration of the container.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointRequest' value with any optional fields omitted.
mkEndpointRequest
    :: Core.Text -- ^ 'containerName'
    -> Core.Int -- ^ 'containerPort'
    -> EndpointRequest
mkEndpointRequest containerName containerPort
  = EndpointRequest'{containerName, containerPort,
                     healthCheck = Core.Nothing}

-- | The name of the container for the endpoint.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erContainerName :: Lens.Lens' EndpointRequest Core.Text
erContainerName = Lens.field @"containerName"
{-# INLINEABLE erContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The port of the container to which traffic is forwarded to.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erContainerPort :: Lens.Lens' EndpointRequest Core.Int
erContainerPort = Lens.field @"containerPort"
{-# INLINEABLE erContainerPort #-}
{-# DEPRECATED containerPort "Use generic-lens or generic-optics with 'containerPort' instead"  #-}

-- | An object that describes the health check configuration of the container.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erHealthCheck :: Lens.Lens' EndpointRequest (Core.Maybe Types.ContainerServiceHealthCheckConfig)
erHealthCheck = Lens.field @"healthCheck"
{-# INLINEABLE erHealthCheck #-}
{-# DEPRECATED healthCheck "Use generic-lens or generic-optics with 'healthCheck' instead"  #-}

instance Core.FromJSON EndpointRequest where
        toJSON EndpointRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerName" Core..= containerName),
                  Core.Just ("containerPort" Core..= containerPort),
                  ("healthCheck" Core..=) Core.<$> healthCheck])
