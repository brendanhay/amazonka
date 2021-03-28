{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Container
  ( Container (..)
  -- * Smart constructor
  , mkContainer
  -- * Lenses
  , cContainerArn
  , cCpu
  , cExitCode
  , cGpuIds
  , cHealthStatus
  , cImage
  , cImageDigest
  , cLastStatus
  , cMemory
  , cMemoryReservation
  , cName
  , cNetworkBindings
  , cNetworkInterfaces
  , cReason
  , cRuntimeId
  , cTaskArn
  ) where

import qualified Network.AWS.ECS.Types.HealthStatus as Types
import qualified Network.AWS.ECS.Types.NetworkBinding as Types
import qualified Network.AWS.ECS.Types.NetworkInterface as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A Docker container that is part of a task.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { containerArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the container.
  , cpu :: Core.Maybe Core.Text
    -- ^ The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
  , exitCode :: Core.Maybe Core.Int
    -- ^ The exit code returned from the container.
  , gpuIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of each GPU assigned to the container.
  , healthStatus :: Core.Maybe Types.HealthStatus
    -- ^ The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
  , image :: Core.Maybe Core.Text
    -- ^ The image used for the container.
  , imageDigest :: Core.Maybe Core.Text
    -- ^ The container image manifest digest.
  , lastStatus :: Core.Maybe Core.Text
    -- ^ The last known status of the container.
  , memory :: Core.Maybe Core.Text
    -- ^ The hard limit (in MiB) of memory set for the container.
  , memoryReservation :: Core.Maybe Core.Text
    -- ^ The soft limit (in MiB) of memory set for the container.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the container.
  , networkBindings :: Core.Maybe [Types.NetworkBinding]
    -- ^ The network bindings associated with the container.
  , networkInterfaces :: Core.Maybe [Types.NetworkInterface]
    -- ^ The network interfaces associated with the container.
  , reason :: Core.Maybe Core.Text
    -- ^ A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
  , runtimeId :: Core.Maybe Core.Text
    -- ^ The ID of the Docker container.
  , taskArn :: Core.Maybe Core.Text
    -- ^ The ARN of the task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Container' value with any optional fields omitted.
mkContainer
    :: Container
mkContainer
  = Container'{containerArn = Core.Nothing, cpu = Core.Nothing,
               exitCode = Core.Nothing, gpuIds = Core.Nothing,
               healthStatus = Core.Nothing, image = Core.Nothing,
               imageDigest = Core.Nothing, lastStatus = Core.Nothing,
               memory = Core.Nothing, memoryReservation = Core.Nothing,
               name = Core.Nothing, networkBindings = Core.Nothing,
               networkInterfaces = Core.Nothing, reason = Core.Nothing,
               runtimeId = Core.Nothing, taskArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the container.
--
-- /Note:/ Consider using 'containerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContainerArn :: Lens.Lens' Container (Core.Maybe Core.Text)
cContainerArn = Lens.field @"containerArn"
{-# INLINEABLE cContainerArn #-}
{-# DEPRECATED containerArn "Use generic-lens or generic-optics with 'containerArn' instead"  #-}

-- | The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCpu :: Lens.Lens' Container (Core.Maybe Core.Text)
cCpu = Lens.field @"cpu"
{-# INLINEABLE cCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The exit code returned from the container.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExitCode :: Lens.Lens' Container (Core.Maybe Core.Int)
cExitCode = Lens.field @"exitCode"
{-# INLINEABLE cExitCode #-}
{-# DEPRECATED exitCode "Use generic-lens or generic-optics with 'exitCode' instead"  #-}

-- | The IDs of each GPU assigned to the container.
--
-- /Note:/ Consider using 'gpuIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGpuIds :: Lens.Lens' Container (Core.Maybe [Core.Text])
cGpuIds = Lens.field @"gpuIds"
{-# INLINEABLE cGpuIds #-}
{-# DEPRECATED gpuIds "Use generic-lens or generic-optics with 'gpuIds' instead"  #-}

-- | The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHealthStatus :: Lens.Lens' Container (Core.Maybe Types.HealthStatus)
cHealthStatus = Lens.field @"healthStatus"
{-# INLINEABLE cHealthStatus #-}
{-# DEPRECATED healthStatus "Use generic-lens or generic-optics with 'healthStatus' instead"  #-}

-- | The image used for the container.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImage :: Lens.Lens' Container (Core.Maybe Core.Text)
cImage = Lens.field @"image"
{-# INLINEABLE cImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The container image manifest digest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImageDigest :: Lens.Lens' Container (Core.Maybe Core.Text)
cImageDigest = Lens.field @"imageDigest"
{-# INLINEABLE cImageDigest #-}
{-# DEPRECATED imageDigest "Use generic-lens or generic-optics with 'imageDigest' instead"  #-}

-- | The last known status of the container.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastStatus :: Lens.Lens' Container (Core.Maybe Core.Text)
cLastStatus = Lens.field @"lastStatus"
{-# INLINEABLE cLastStatus #-}
{-# DEPRECATED lastStatus "Use generic-lens or generic-optics with 'lastStatus' instead"  #-}

-- | The hard limit (in MiB) of memory set for the container.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMemory :: Lens.Lens' Container (Core.Maybe Core.Text)
cMemory = Lens.field @"memory"
{-# INLINEABLE cMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The soft limit (in MiB) of memory set for the container.
--
-- /Note:/ Consider using 'memoryReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMemoryReservation :: Lens.Lens' Container (Core.Maybe Core.Text)
cMemoryReservation = Lens.field @"memoryReservation"
{-# INLINEABLE cMemoryReservation #-}
{-# DEPRECATED memoryReservation "Use generic-lens or generic-optics with 'memoryReservation' instead"  #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Container (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The network bindings associated with the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNetworkBindings :: Lens.Lens' Container (Core.Maybe [Types.NetworkBinding])
cNetworkBindings = Lens.field @"networkBindings"
{-# INLINEABLE cNetworkBindings #-}
{-# DEPRECATED networkBindings "Use generic-lens or generic-optics with 'networkBindings' instead"  #-}

-- | The network interfaces associated with the container.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNetworkInterfaces :: Lens.Lens' Container (Core.Maybe [Types.NetworkInterface])
cNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE cNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReason :: Lens.Lens' Container (Core.Maybe Core.Text)
cReason = Lens.field @"reason"
{-# INLINEABLE cReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRuntimeId :: Lens.Lens' Container (Core.Maybe Core.Text)
cRuntimeId = Lens.field @"runtimeId"
{-# INLINEABLE cRuntimeId #-}
{-# DEPRECATED runtimeId "Use generic-lens or generic-optics with 'runtimeId' instead"  #-}

-- | The ARN of the task.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTaskArn :: Lens.Lens' Container (Core.Maybe Core.Text)
cTaskArn = Lens.field @"taskArn"
{-# INLINEABLE cTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

instance Core.FromJSON Container where
        parseJSON
          = Core.withObject "Container" Core.$
              \ x ->
                Container' Core.<$>
                  (x Core..:? "containerArn") Core.<*> x Core..:? "cpu" Core.<*>
                    x Core..:? "exitCode"
                    Core.<*> x Core..:? "gpuIds"
                    Core.<*> x Core..:? "healthStatus"
                    Core.<*> x Core..:? "image"
                    Core.<*> x Core..:? "imageDigest"
                    Core.<*> x Core..:? "lastStatus"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "memoryReservation"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "networkBindings"
                    Core.<*> x Core..:? "networkInterfaces"
                    Core.<*> x Core..:? "reason"
                    Core.<*> x Core..:? "runtimeId"
                    Core.<*> x Core..:? "taskArn"
