{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LinuxParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LinuxParameters where

import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.Tmpfs
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Linux-specific options that are applied to the container, such as Linux
-- KernelCapabilities.
--
-- /See:/ 'newLinuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { -- | The container path, mount options, and size (in MiB) of the tmpfs mount.
    -- This parameter maps to the @--tmpfs@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If you are using tasks that use the Fargate launch type, the @tmpfs@
    -- parameter is not supported.
    tmpfs :: Prelude.Maybe [Tmpfs],
    -- | The total amount of swap memory (in MiB) a container can use. This
    -- parameter will be translated to the @--memory-swap@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>
    -- where the value would be the sum of the container memory plus the
    -- @maxSwap@ value.
    --
    -- If a @maxSwap@ value of @0@ is specified, the container will not use
    -- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
    -- parameter is omitted, the container will use the swap configuration for
    -- the container instance it is running on. A @maxSwap@ value must be set
    -- for the @swappiness@ parameter to be used.
    --
    -- If you are using tasks that use the Fargate launch type, the @maxSwap@
    -- parameter is not supported.
    maxSwap :: Prelude.Maybe Prelude.Int,
    -- | The Linux capabilities for the container that are added to or dropped
    -- from the default configuration provided by Docker.
    --
    -- For tasks that use the Fargate launch type, @capabilities@ is supported
    -- for all platform versions but the @add@ parameter is only supported if
    -- using platform version 1.4.0 or later.
    capabilities :: Prelude.Maybe KernelCapabilities,
    -- | Any host devices to expose to the container. This parameter maps to
    -- @Devices@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--device@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If you are using tasks that use the Fargate launch type, the @devices@
    -- parameter is not supported.
    devices :: Prelude.Maybe [Device],
    -- | This allows you to tune a container\'s memory swappiness behavior. A
    -- @swappiness@ value of @0@ will cause swapping to not happen unless
    -- absolutely necessary. A @swappiness@ value of @100@ will cause pages to
    -- be swapped very aggressively. Accepted values are whole numbers between
    -- @0@ and @100@. If the @swappiness@ parameter is not specified, a default
    -- value of @60@ is used. If a value is not specified for @maxSwap@ then
    -- this parameter is ignored. This parameter maps to the
    -- @--memory-swappiness@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If you are using tasks that use the Fargate launch type, the
    -- @swappiness@ parameter is not supported.
    swappiness :: Prelude.Maybe Prelude.Int,
    -- | Run an @init@ process inside the container that forwards signals and
    -- reaps processes. This parameter maps to the @--init@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- This parameter requires version 1.25 of the Docker Remote API or greater
    -- on your container instance. To check the Docker Remote API version on
    -- your container instance, log in to your container instance and run the
    -- following command:
    -- @sudo docker version --format \'{{.Server.APIVersion}}\'@
    initProcessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The value for the size (in MiB) of the @\/dev\/shm@ volume. This
    -- parameter maps to the @--shm-size@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If you are using tasks that use the Fargate launch type, the
    -- @sharedMemorySize@ parameter is not supported.
    sharedMemorySize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LinuxParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tmpfs', 'linuxParameters_tmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount.
-- This parameter maps to the @--tmpfs@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the @tmpfs@
-- parameter is not supported.
--
-- 'maxSwap', 'linuxParameters_maxSwap' - The total amount of swap memory (in MiB) a container can use. This
-- parameter will be translated to the @--memory-swap@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>
-- where the value would be the sum of the container memory plus the
-- @maxSwap@ value.
--
-- If a @maxSwap@ value of @0@ is specified, the container will not use
-- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
-- parameter is omitted, the container will use the swap configuration for
-- the container instance it is running on. A @maxSwap@ value must be set
-- for the @swappiness@ parameter to be used.
--
-- If you are using tasks that use the Fargate launch type, the @maxSwap@
-- parameter is not supported.
--
-- 'capabilities', 'linuxParameters_capabilities' - The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- For tasks that use the Fargate launch type, @capabilities@ is supported
-- for all platform versions but the @add@ parameter is only supported if
-- using platform version 1.4.0 or later.
--
-- 'devices', 'linuxParameters_devices' - Any host devices to expose to the container. This parameter maps to
-- @Devices@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--device@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the @devices@
-- parameter is not supported.
--
-- 'swappiness', 'linuxParameters_swappiness' - This allows you to tune a container\'s memory swappiness behavior. A
-- @swappiness@ value of @0@ will cause swapping to not happen unless
-- absolutely necessary. A @swappiness@ value of @100@ will cause pages to
-- be swapped very aggressively. Accepted values are whole numbers between
-- @0@ and @100@. If the @swappiness@ parameter is not specified, a default
-- value of @60@ is used. If a value is not specified for @maxSwap@ then
-- this parameter is ignored. This parameter maps to the
-- @--memory-swappiness@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the
-- @swappiness@ parameter is not supported.
--
-- 'initProcessEnabled', 'linuxParameters_initProcessEnabled' - Run an @init@ process inside the container that forwards signals and
-- reaps processes. This parameter maps to the @--init@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- This parameter requires version 1.25 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- 'sharedMemorySize', 'linuxParameters_sharedMemorySize' - The value for the size (in MiB) of the @\/dev\/shm@ volume. This
-- parameter maps to the @--shm-size@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the
-- @sharedMemorySize@ parameter is not supported.
newLinuxParameters ::
  LinuxParameters
newLinuxParameters =
  LinuxParameters'
    { tmpfs = Prelude.Nothing,
      maxSwap = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      devices = Prelude.Nothing,
      swappiness = Prelude.Nothing,
      initProcessEnabled = Prelude.Nothing,
      sharedMemorySize = Prelude.Nothing
    }

-- | The container path, mount options, and size (in MiB) of the tmpfs mount.
-- This parameter maps to the @--tmpfs@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the @tmpfs@
-- parameter is not supported.
linuxParameters_tmpfs :: Lens.Lens' LinuxParameters (Prelude.Maybe [Tmpfs])
linuxParameters_tmpfs = Lens.lens (\LinuxParameters' {tmpfs} -> tmpfs) (\s@LinuxParameters' {} a -> s {tmpfs = a} :: LinuxParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | The total amount of swap memory (in MiB) a container can use. This
-- parameter will be translated to the @--memory-swap@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>
-- where the value would be the sum of the container memory plus the
-- @maxSwap@ value.
--
-- If a @maxSwap@ value of @0@ is specified, the container will not use
-- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
-- parameter is omitted, the container will use the swap configuration for
-- the container instance it is running on. A @maxSwap@ value must be set
-- for the @swappiness@ parameter to be used.
--
-- If you are using tasks that use the Fargate launch type, the @maxSwap@
-- parameter is not supported.
linuxParameters_maxSwap :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Int)
linuxParameters_maxSwap = Lens.lens (\LinuxParameters' {maxSwap} -> maxSwap) (\s@LinuxParameters' {} a -> s {maxSwap = a} :: LinuxParameters)

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- For tasks that use the Fargate launch type, @capabilities@ is supported
-- for all platform versions but the @add@ parameter is only supported if
-- using platform version 1.4.0 or later.
linuxParameters_capabilities :: Lens.Lens' LinuxParameters (Prelude.Maybe KernelCapabilities)
linuxParameters_capabilities = Lens.lens (\LinuxParameters' {capabilities} -> capabilities) (\s@LinuxParameters' {} a -> s {capabilities = a} :: LinuxParameters)

-- | Any host devices to expose to the container. This parameter maps to
-- @Devices@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--device@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the @devices@
-- parameter is not supported.
linuxParameters_devices :: Lens.Lens' LinuxParameters (Prelude.Maybe [Device])
linuxParameters_devices = Lens.lens (\LinuxParameters' {devices} -> devices) (\s@LinuxParameters' {} a -> s {devices = a} :: LinuxParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | This allows you to tune a container\'s memory swappiness behavior. A
-- @swappiness@ value of @0@ will cause swapping to not happen unless
-- absolutely necessary. A @swappiness@ value of @100@ will cause pages to
-- be swapped very aggressively. Accepted values are whole numbers between
-- @0@ and @100@. If the @swappiness@ parameter is not specified, a default
-- value of @60@ is used. If a value is not specified for @maxSwap@ then
-- this parameter is ignored. This parameter maps to the
-- @--memory-swappiness@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the
-- @swappiness@ parameter is not supported.
linuxParameters_swappiness :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Int)
linuxParameters_swappiness = Lens.lens (\LinuxParameters' {swappiness} -> swappiness) (\s@LinuxParameters' {} a -> s {swappiness = a} :: LinuxParameters)

-- | Run an @init@ process inside the container that forwards signals and
-- reaps processes. This parameter maps to the @--init@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- This parameter requires version 1.25 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
linuxParameters_initProcessEnabled :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Bool)
linuxParameters_initProcessEnabled = Lens.lens (\LinuxParameters' {initProcessEnabled} -> initProcessEnabled) (\s@LinuxParameters' {} a -> s {initProcessEnabled = a} :: LinuxParameters)

-- | The value for the size (in MiB) of the @\/dev\/shm@ volume. This
-- parameter maps to the @--shm-size@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If you are using tasks that use the Fargate launch type, the
-- @sharedMemorySize@ parameter is not supported.
linuxParameters_sharedMemorySize :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Int)
linuxParameters_sharedMemorySize = Lens.lens (\LinuxParameters' {sharedMemorySize} -> sharedMemorySize) (\s@LinuxParameters' {} a -> s {sharedMemorySize = a} :: LinuxParameters)

instance Prelude.FromJSON LinuxParameters where
  parseJSON =
    Prelude.withObject
      "LinuxParameters"
      ( \x ->
          LinuxParameters'
            Prelude.<$> (x Prelude..:? "tmpfs" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "maxSwap")
            Prelude.<*> (x Prelude..:? "capabilities")
            Prelude.<*> (x Prelude..:? "devices" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "swappiness")
            Prelude.<*> (x Prelude..:? "initProcessEnabled")
            Prelude.<*> (x Prelude..:? "sharedMemorySize")
      )

instance Prelude.Hashable LinuxParameters

instance Prelude.NFData LinuxParameters

instance Prelude.ToJSON LinuxParameters where
  toJSON LinuxParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tmpfs" Prelude..=) Prelude.<$> tmpfs,
            ("maxSwap" Prelude..=) Prelude.<$> maxSwap,
            ("capabilities" Prelude..=) Prelude.<$> capabilities,
            ("devices" Prelude..=) Prelude.<$> devices,
            ("swappiness" Prelude..=) Prelude.<$> swappiness,
            ("initProcessEnabled" Prelude..=)
              Prelude.<$> initProcessEnabled,
            ("sharedMemorySize" Prelude..=)
              Prelude.<$> sharedMemorySize
          ]
      )
