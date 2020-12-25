{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LinuxParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LinuxParameters
  ( LinuxParameters (..),

    -- * Smart constructor
    mkLinuxParameters,

    -- * Lenses
    lpCapabilities,
    lpDevices,
    lpInitProcessEnabled,
    lpMaxSwap,
    lpSharedMemorySize,
    lpSwappiness,
    lpTmpfs,
  )
where

import qualified Network.AWS.ECS.Types.Device as Types
import qualified Network.AWS.ECS.Types.KernelCapabilities as Types
import qualified Network.AWS.ECS.Types.Tmpfs as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Linux-specific options that are applied to the container, such as Linux 'KernelCapabilities' .
--
-- /See:/ 'mkLinuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { -- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
    capabilities :: Core.Maybe Types.KernelCapabilities,
    -- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
    devices :: Core.Maybe [Types.Device],
    -- | Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
    initProcessEnabled :: Core.Maybe Core.Bool,
    -- | The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> where the value would be the sum of the container memory plus the @maxSwap@ value.
    --
    -- If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
    maxSwap :: Core.Maybe Core.Int,
    -- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
    sharedMemorySize :: Core.Maybe Core.Int,
    -- | This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
    swappiness :: Core.Maybe Core.Int,
    -- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
    tmpfs :: Core.Maybe [Types.Tmpfs]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LinuxParameters' value with any optional fields omitted.
mkLinuxParameters ::
  LinuxParameters
mkLinuxParameters =
  LinuxParameters'
    { capabilities = Core.Nothing,
      devices = Core.Nothing,
      initProcessEnabled = Core.Nothing,
      maxSwap = Core.Nothing,
      sharedMemorySize = Core.Nothing,
      swappiness = Core.Nothing,
      tmpfs = Core.Nothing
    }

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCapabilities :: Lens.Lens' LinuxParameters (Core.Maybe Types.KernelCapabilities)
lpCapabilities = Lens.field @"capabilities"
{-# DEPRECATED lpCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpDevices :: Lens.Lens' LinuxParameters (Core.Maybe [Types.Device])
lpDevices = Lens.field @"devices"
{-# DEPRECATED lpDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- /Note:/ Consider using 'initProcessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpInitProcessEnabled :: Lens.Lens' LinuxParameters (Core.Maybe Core.Bool)
lpInitProcessEnabled = Lens.field @"initProcessEnabled"
{-# DEPRECATED lpInitProcessEnabled "Use generic-lens or generic-optics with 'initProcessEnabled' instead." #-}

-- | The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> where the value would be the sum of the container memory plus the @maxSwap@ value.
--
-- If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
--
-- /Note:/ Consider using 'maxSwap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxSwap :: Lens.Lens' LinuxParameters (Core.Maybe Core.Int)
lpMaxSwap = Lens.field @"maxSwap"
{-# DEPRECATED lpMaxSwap "Use generic-lens or generic-optics with 'maxSwap' instead." #-}

-- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'sharedMemorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSharedMemorySize :: Lens.Lens' LinuxParameters (Core.Maybe Core.Int)
lpSharedMemorySize = Lens.field @"sharedMemorySize"
{-# DEPRECATED lpSharedMemorySize "Use generic-lens or generic-optics with 'sharedMemorySize' instead." #-}

-- | This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'swappiness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSwappiness :: Lens.Lens' LinuxParameters (Core.Maybe Core.Int)
lpSwappiness = Lens.field @"swappiness"
{-# DEPRECATED lpSwappiness "Use generic-lens or generic-optics with 'swappiness' instead." #-}

-- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'tmpfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpTmpfs :: Lens.Lens' LinuxParameters (Core.Maybe [Types.Tmpfs])
lpTmpfs = Lens.field @"tmpfs"
{-# DEPRECATED lpTmpfs "Use generic-lens or generic-optics with 'tmpfs' instead." #-}

instance Core.FromJSON LinuxParameters where
  toJSON LinuxParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("capabilities" Core..=) Core.<$> capabilities,
            ("devices" Core..=) Core.<$> devices,
            ("initProcessEnabled" Core..=) Core.<$> initProcessEnabled,
            ("maxSwap" Core..=) Core.<$> maxSwap,
            ("sharedMemorySize" Core..=) Core.<$> sharedMemorySize,
            ("swappiness" Core..=) Core.<$> swappiness,
            ("tmpfs" Core..=) Core.<$> tmpfs
          ]
      )

instance Core.FromJSON LinuxParameters where
  parseJSON =
    Core.withObject "LinuxParameters" Core.$
      \x ->
        LinuxParameters'
          Core.<$> (x Core..:? "capabilities")
          Core.<*> (x Core..:? "devices")
          Core.<*> (x Core..:? "initProcessEnabled")
          Core.<*> (x Core..:? "maxSwap")
          Core.<*> (x Core..:? "sharedMemorySize")
          Core.<*> (x Core..:? "swappiness")
          Core.<*> (x Core..:? "tmpfs")
