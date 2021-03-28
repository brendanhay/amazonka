{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ServiceRegistry
  ( ServiceRegistry (..)
  -- * Smart constructor
  , mkServiceRegistry
  -- * Lenses
  , srContainerName
  , srContainerPort
  , srPort
  , srRegistryArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the service registry.
--
-- /See:/ 'mkServiceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { containerName :: Core.Maybe Core.Text
    -- ^ The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
  , containerPort :: Core.Maybe Core.Int
    -- ^ The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
  , port :: Core.Maybe Core.Int
    -- ^ The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
  , registryArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceRegistry' value with any optional fields omitted.
mkServiceRegistry
    :: ServiceRegistry
mkServiceRegistry
  = ServiceRegistry'{containerName = Core.Nothing,
                     containerPort = Core.Nothing, port = Core.Nothing,
                     registryArn = Core.Nothing}

-- | The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srContainerName :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Text)
srContainerName = Lens.field @"containerName"
{-# INLINEABLE srContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srContainerPort :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Int)
srContainerPort = Lens.field @"containerPort"
{-# INLINEABLE srContainerPort #-}
{-# DEPRECATED containerPort "Use generic-lens or generic-optics with 'containerPort' instead"  #-}

-- | The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPort :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Int)
srPort = Lens.field @"port"
{-# INLINEABLE srPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is AWS Cloud Map. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService> .
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRegistryArn :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Text)
srRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE srRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

instance Core.FromJSON ServiceRegistry where
        toJSON ServiceRegistry{..}
          = Core.object
              (Core.catMaybes
                 [("containerName" Core..=) Core.<$> containerName,
                  ("containerPort" Core..=) Core.<$> containerPort,
                  ("port" Core..=) Core.<$> port,
                  ("registryArn" Core..=) Core.<$> registryArn])

instance Core.FromJSON ServiceRegistry where
        parseJSON
          = Core.withObject "ServiceRegistry" Core.$
              \ x ->
                ServiceRegistry' Core.<$>
                  (x Core..:? "containerName") Core.<*> x Core..:? "containerPort"
                    Core.<*> x Core..:? "port"
                    Core.<*> x Core..:? "registryArn"
