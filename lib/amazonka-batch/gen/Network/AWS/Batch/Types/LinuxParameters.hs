{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LinuxParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LinuxParameters
  ( LinuxParameters (..),

    -- * Smart constructor
    mkLinuxParameters,

    -- * Lenses
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpMaxSwap,
  )
where

import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.Tmpfs
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- /See:/ 'mkLinuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { -- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
    sharedMemorySize :: Lude.Maybe Lude.Int,
    -- | If true, run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
    initProcessEnabled :: Lude.Maybe Lude.Bool,
    -- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
    tmpfs :: Lude.Maybe [Tmpfs],
    -- | This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
    swappiness :: Lude.Maybe Lude.Int,
    -- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
    devices :: Lude.Maybe [Device],
    -- | The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/ docker run> where the value would be the sum of the container memory plus the @maxSwap@ value. For more information, see <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details @--memory-swap@ details> in the Docker documentation.
    --
    -- If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
    maxSwap :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LinuxParameters' with the minimum fields required to make a request.
--
-- * 'sharedMemorySize' - The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'initProcessEnabled' - If true, run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
-- * 'tmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'swappiness' - This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'devices' - Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'maxSwap' - The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/ docker run> where the value would be the sum of the container memory plus the @maxSwap@ value. For more information, see <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details @--memory-swap@ details> in the Docker documentation.
--
-- If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
mkLinuxParameters ::
  LinuxParameters
mkLinuxParameters =
  LinuxParameters'
    { sharedMemorySize = Lude.Nothing,
      initProcessEnabled = Lude.Nothing,
      tmpfs = Lude.Nothing,
      swappiness = Lude.Nothing,
      devices = Lude.Nothing,
      maxSwap = Lude.Nothing
    }

-- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'sharedMemorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSharedMemorySize :: Lens.Lens' LinuxParameters (Lude.Maybe Lude.Int)
lpSharedMemorySize = Lens.lens (sharedMemorySize :: LinuxParameters -> Lude.Maybe Lude.Int) (\s a -> s {sharedMemorySize = a} :: LinuxParameters)
{-# DEPRECATED lpSharedMemorySize "Use generic-lens or generic-optics with 'sharedMemorySize' instead." #-}

-- | If true, run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- /Note:/ Consider using 'initProcessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpInitProcessEnabled :: Lens.Lens' LinuxParameters (Lude.Maybe Lude.Bool)
lpInitProcessEnabled = Lens.lens (initProcessEnabled :: LinuxParameters -> Lude.Maybe Lude.Bool) (\s a -> s {initProcessEnabled = a} :: LinuxParameters)
{-# DEPRECATED lpInitProcessEnabled "Use generic-lens or generic-optics with 'initProcessEnabled' instead." #-}

-- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'tmpfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpTmpfs :: Lens.Lens' LinuxParameters (Lude.Maybe [Tmpfs])
lpTmpfs = Lens.lens (tmpfs :: LinuxParameters -> Lude.Maybe [Tmpfs]) (\s a -> s {tmpfs = a} :: LinuxParameters)
{-# DEPRECATED lpTmpfs "Use generic-lens or generic-optics with 'tmpfs' instead." #-}

-- | This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'swappiness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSwappiness :: Lens.Lens' LinuxParameters (Lude.Maybe Lude.Int)
lpSwappiness = Lens.lens (swappiness :: LinuxParameters -> Lude.Maybe Lude.Int) (\s a -> s {swappiness = a} :: LinuxParameters)
{-# DEPRECATED lpSwappiness "Use generic-lens or generic-optics with 'swappiness' instead." #-}

-- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpDevices :: Lens.Lens' LinuxParameters (Lude.Maybe [Device])
lpDevices = Lens.lens (devices :: LinuxParameters -> Lude.Maybe [Device]) (\s a -> s {devices = a} :: LinuxParameters)
{-# DEPRECATED lpDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/ docker run> where the value would be the sum of the container memory plus the @maxSwap@ value. For more information, see <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details @--memory-swap@ details> in the Docker documentation.
--
-- If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
--
-- /Note:/ Consider using 'maxSwap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxSwap :: Lens.Lens' LinuxParameters (Lude.Maybe Lude.Int)
lpMaxSwap = Lens.lens (maxSwap :: LinuxParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxSwap = a} :: LinuxParameters)
{-# DEPRECATED lpMaxSwap "Use generic-lens or generic-optics with 'maxSwap' instead." #-}

instance Lude.FromJSON LinuxParameters where
  parseJSON =
    Lude.withObject
      "LinuxParameters"
      ( \x ->
          LinuxParameters'
            Lude.<$> (x Lude..:? "sharedMemorySize")
            Lude.<*> (x Lude..:? "initProcessEnabled")
            Lude.<*> (x Lude..:? "tmpfs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "swappiness")
            Lude.<*> (x Lude..:? "devices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "maxSwap")
      )

instance Lude.ToJSON LinuxParameters where
  toJSON LinuxParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sharedMemorySize" Lude..=) Lude.<$> sharedMemorySize,
            ("initProcessEnabled" Lude..=) Lude.<$> initProcessEnabled,
            ("tmpfs" Lude..=) Lude.<$> tmpfs,
            ("swappiness" Lude..=) Lude.<$> swappiness,
            ("devices" Lude..=) Lude.<$> devices,
            ("maxSwap" Lude..=) Lude.<$> maxSwap
          ]
      )
