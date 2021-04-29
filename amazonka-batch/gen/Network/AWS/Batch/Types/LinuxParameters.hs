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
-- Module      : Network.AWS.Batch.Types.LinuxParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LinuxParameters where

import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.Tmpfs
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
--
-- /See:/ 'newLinuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { -- | The container path, mount options, and size (in MiB) of the tmpfs mount.
    -- This parameter maps to the @--tmpfs@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    tmpfs :: Prelude.Maybe [Tmpfs],
    -- | The total amount of swap memory (in MiB) a container can use. This
    -- parameter is translated to the @--memory-swap@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run> where the
    -- value is the sum of the container memory plus the @maxSwap@ value. For
    -- more information, see
    -- <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details --memory-swap details>
    -- in the Docker documentation.
    --
    -- If a @maxSwap@ value of @0@ is specified, the container doesn\'t use
    -- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
    -- parameter is omitted, the container doesn\'t use the swap configuration
    -- for the container instance it is running on. A @maxSwap@ value must be
    -- set for the @swappiness@ parameter to be used.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    maxSwap :: Prelude.Maybe Prelude.Int,
    -- | Any host devices to expose to the container. This parameter maps to
    -- @Devices@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--device@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    devices :: Prelude.Maybe [Device],
    -- | This allows you to tune a container\'s memory swappiness behavior. A
    -- @swappiness@ value of @0@ causes swapping not to happen unless
    -- absolutely necessary. A @swappiness@ value of @100@ causes pages to be
    -- swapped very aggressively. Accepted values are whole numbers between @0@
    -- and @100@. If the @swappiness@ parameter isn\'t specified, a default
    -- value of @60@ is used. If a value isn\'t specified for @maxSwap@ then
    -- this parameter is ignored. If @maxSwap@ is set to 0, the container
    -- doesn\'t use swap. This parameter maps to the @--memory-swappiness@
    -- option to <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- Consider the following when you use a per-container swap configuration.
    --
    -- -   Swap space must be enabled and allocated on the container instance
    --     for the containers to use.
    --
    --     The Amazon ECS optimized AMIs don\'t have swap enabled by default.
    --     You must enable swap on the instance to use this feature. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-store-swap-volumes.html Instance Store Swap Volumes>
    --     in the /Amazon EC2 User Guide for Linux Instances/ or
    --     <http://aws.amazon.com/premiumsupport/knowledge-center/ec2-memory-swap-file/ How do I allocate memory to work as swap space in an Amazon EC2 instance by using a swap file?>
    --
    -- -   The swap space parameters are only supported for job definitions
    --     using EC2 resources.
    --
    -- -   If the @maxSwap@ and @swappiness@ parameters are omitted from a job
    --     definition, each container will have a default @swappiness@ value of
    --     60 and the total swap usage will be limited to two times the memory
    --     reservation of the container.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    swappiness :: Prelude.Maybe Prelude.Int,
    -- | If true, run an @init@ process inside the container that forwards
    -- signals and reaps processes. This parameter maps to the @--init@ option
    -- to <https://docs.docker.com/engine/reference/run/ docker run>. This
    -- parameter requires version 1.25 of the Docker Remote API or greater on
    -- your container instance. To check the Docker Remote API version on your
    -- container instance, log into your container instance and run the
    -- following command: @sudo docker version | grep \"Server API version\"@
    initProcessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The value for the size (in MiB) of the @\/dev\/shm@ volume. This
    -- parameter maps to the @--shm-size@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
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
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
--
-- 'maxSwap', 'linuxParameters_maxSwap' - The total amount of swap memory (in MiB) a container can use. This
-- parameter is translated to the @--memory-swap@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run> where the
-- value is the sum of the container memory plus the @maxSwap@ value. For
-- more information, see
-- <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details --memory-swap details>
-- in the Docker documentation.
--
-- If a @maxSwap@ value of @0@ is specified, the container doesn\'t use
-- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
-- parameter is omitted, the container doesn\'t use the swap configuration
-- for the container instance it is running on. A @maxSwap@ value must be
-- set for the @swappiness@ parameter to be used.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
--
-- 'devices', 'linuxParameters_devices' - Any host devices to expose to the container. This parameter maps to
-- @Devices@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--device@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
--
-- 'swappiness', 'linuxParameters_swappiness' - This allows you to tune a container\'s memory swappiness behavior. A
-- @swappiness@ value of @0@ causes swapping not to happen unless
-- absolutely necessary. A @swappiness@ value of @100@ causes pages to be
-- swapped very aggressively. Accepted values are whole numbers between @0@
-- and @100@. If the @swappiness@ parameter isn\'t specified, a default
-- value of @60@ is used. If a value isn\'t specified for @maxSwap@ then
-- this parameter is ignored. If @maxSwap@ is set to 0, the container
-- doesn\'t use swap. This parameter maps to the @--memory-swappiness@
-- option to <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- Consider the following when you use a per-container swap configuration.
--
-- -   Swap space must be enabled and allocated on the container instance
--     for the containers to use.
--
--     The Amazon ECS optimized AMIs don\'t have swap enabled by default.
--     You must enable swap on the instance to use this feature. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-store-swap-volumes.html Instance Store Swap Volumes>
--     in the /Amazon EC2 User Guide for Linux Instances/ or
--     <http://aws.amazon.com/premiumsupport/knowledge-center/ec2-memory-swap-file/ How do I allocate memory to work as swap space in an Amazon EC2 instance by using a swap file?>
--
-- -   The swap space parameters are only supported for job definitions
--     using EC2 resources.
--
-- -   If the @maxSwap@ and @swappiness@ parameters are omitted from a job
--     definition, each container will have a default @swappiness@ value of
--     60 and the total swap usage will be limited to two times the memory
--     reservation of the container.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
--
-- 'initProcessEnabled', 'linuxParameters_initProcessEnabled' - If true, run an @init@ process inside the container that forwards
-- signals and reaps processes. This parameter maps to the @--init@ option
-- to <https://docs.docker.com/engine/reference/run/ docker run>. This
-- parameter requires version 1.25 of the Docker Remote API or greater on
-- your container instance. To check the Docker Remote API version on your
-- container instance, log into your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- 'sharedMemorySize', 'linuxParameters_sharedMemorySize' - The value for the size (in MiB) of the @\/dev\/shm@ volume. This
-- parameter maps to the @--shm-size@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
newLinuxParameters ::
  LinuxParameters
newLinuxParameters =
  LinuxParameters'
    { tmpfs = Prelude.Nothing,
      maxSwap = Prelude.Nothing,
      devices = Prelude.Nothing,
      swappiness = Prelude.Nothing,
      initProcessEnabled = Prelude.Nothing,
      sharedMemorySize = Prelude.Nothing
    }

-- | The container path, mount options, and size (in MiB) of the tmpfs mount.
-- This parameter maps to the @--tmpfs@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
linuxParameters_tmpfs :: Lens.Lens' LinuxParameters (Prelude.Maybe [Tmpfs])
linuxParameters_tmpfs = Lens.lens (\LinuxParameters' {tmpfs} -> tmpfs) (\s@LinuxParameters' {} a -> s {tmpfs = a} :: LinuxParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | The total amount of swap memory (in MiB) a container can use. This
-- parameter is translated to the @--memory-swap@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run> where the
-- value is the sum of the container memory plus the @maxSwap@ value. For
-- more information, see
-- <https://docs.docker.com/config/containers/resource_constraints/#--memory-swap-details --memory-swap details>
-- in the Docker documentation.
--
-- If a @maxSwap@ value of @0@ is specified, the container doesn\'t use
-- swap. Accepted values are @0@ or any positive integer. If the @maxSwap@
-- parameter is omitted, the container doesn\'t use the swap configuration
-- for the container instance it is running on. A @maxSwap@ value must be
-- set for the @swappiness@ parameter to be used.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
linuxParameters_maxSwap :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Int)
linuxParameters_maxSwap = Lens.lens (\LinuxParameters' {maxSwap} -> maxSwap) (\s@LinuxParameters' {} a -> s {maxSwap = a} :: LinuxParameters)

-- | Any host devices to expose to the container. This parameter maps to
-- @Devices@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--device@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
linuxParameters_devices :: Lens.Lens' LinuxParameters (Prelude.Maybe [Device])
linuxParameters_devices = Lens.lens (\LinuxParameters' {devices} -> devices) (\s@LinuxParameters' {} a -> s {devices = a} :: LinuxParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | This allows you to tune a container\'s memory swappiness behavior. A
-- @swappiness@ value of @0@ causes swapping not to happen unless
-- absolutely necessary. A @swappiness@ value of @100@ causes pages to be
-- swapped very aggressively. Accepted values are whole numbers between @0@
-- and @100@. If the @swappiness@ parameter isn\'t specified, a default
-- value of @60@ is used. If a value isn\'t specified for @maxSwap@ then
-- this parameter is ignored. If @maxSwap@ is set to 0, the container
-- doesn\'t use swap. This parameter maps to the @--memory-swappiness@
-- option to <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- Consider the following when you use a per-container swap configuration.
--
-- -   Swap space must be enabled and allocated on the container instance
--     for the containers to use.
--
--     The Amazon ECS optimized AMIs don\'t have swap enabled by default.
--     You must enable swap on the instance to use this feature. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-store-swap-volumes.html Instance Store Swap Volumes>
--     in the /Amazon EC2 User Guide for Linux Instances/ or
--     <http://aws.amazon.com/premiumsupport/knowledge-center/ec2-memory-swap-file/ How do I allocate memory to work as swap space in an Amazon EC2 instance by using a swap file?>
--
-- -   The swap space parameters are only supported for job definitions
--     using EC2 resources.
--
-- -   If the @maxSwap@ and @swappiness@ parameters are omitted from a job
--     definition, each container will have a default @swappiness@ value of
--     60 and the total swap usage will be limited to two times the memory
--     reservation of the container.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
linuxParameters_swappiness :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Int)
linuxParameters_swappiness = Lens.lens (\LinuxParameters' {swappiness} -> swappiness) (\s@LinuxParameters' {} a -> s {swappiness = a} :: LinuxParameters)

-- | If true, run an @init@ process inside the container that forwards
-- signals and reaps processes. This parameter maps to the @--init@ option
-- to <https://docs.docker.com/engine/reference/run/ docker run>. This
-- parameter requires version 1.25 of the Docker Remote API or greater on
-- your container instance. To check the Docker Remote API version on your
-- container instance, log into your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
linuxParameters_initProcessEnabled :: Lens.Lens' LinuxParameters (Prelude.Maybe Prelude.Bool)
linuxParameters_initProcessEnabled = Lens.lens (\LinuxParameters' {initProcessEnabled} -> initProcessEnabled) (\s@LinuxParameters' {} a -> s {initProcessEnabled = a} :: LinuxParameters)

-- | The value for the size (in MiB) of the @\/dev\/shm@ volume. This
-- parameter maps to the @--shm-size@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
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
            ("devices" Prelude..=) Prelude.<$> devices,
            ("swappiness" Prelude..=) Prelude.<$> swappiness,
            ("initProcessEnabled" Prelude..=)
              Prelude.<$> initProcessEnabled,
            ("sharedMemorySize" Prelude..=)
              Prelude.<$> sharedMemorySize
          ]
      )
