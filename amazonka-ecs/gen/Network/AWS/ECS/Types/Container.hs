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
-- Module      : Network.AWS.ECS.Types.Container
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Container where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkInterface
import qualified Network.AWS.Lens as Lens

-- | A Docker container that is part of a task.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The container image manifest digest.
    --
    -- The @imageDigest@ is only returned if the container is using an image
    -- hosted in Amazon ECR, otherwise it is omitted.
    imageDigest :: Core.Maybe Core.Text,
    -- | The IDs of each GPU assigned to the container.
    gpuIds :: Core.Maybe [Core.Text],
    -- | The hard limit (in MiB) of memory set for the container.
    memory :: Core.Maybe Core.Text,
    -- | The soft limit (in MiB) of memory set for the container.
    memoryReservation :: Core.Maybe Core.Text,
    -- | The ID of the Docker container.
    runtimeId :: Core.Maybe Core.Text,
    -- | The exit code returned from the container.
    exitCode :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the container.
    containerArn :: Core.Maybe Core.Text,
    -- | The name of the container.
    name :: Core.Maybe Core.Text,
    -- | The image used for the container.
    image :: Core.Maybe Core.Text,
    -- | The network bindings associated with the container.
    networkBindings :: Core.Maybe [NetworkBinding],
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Core.Maybe Core.Text,
    -- | The number of CPU units set for the container. The value will be @0@ if
    -- no value was specified in the container definition when the task
    -- definition was registered.
    cpu :: Core.Maybe Core.Text,
    -- | The last known status of the container.
    lastStatus :: Core.Maybe Core.Text,
    -- | The ARN of the task.
    taskArn :: Core.Maybe Core.Text,
    -- | The health status of the container. If health checks are not configured
    -- for this container in its task definition, then it reports the health
    -- status as @UNKNOWN@.
    healthStatus :: Core.Maybe HealthStatus,
    -- | The network interfaces associated with the container.
    networkInterfaces :: Core.Maybe [NetworkInterface]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'container_imageDigest' - The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
--
-- 'gpuIds', 'container_gpuIds' - The IDs of each GPU assigned to the container.
--
-- 'memory', 'container_memory' - The hard limit (in MiB) of memory set for the container.
--
-- 'memoryReservation', 'container_memoryReservation' - The soft limit (in MiB) of memory set for the container.
--
-- 'runtimeId', 'container_runtimeId' - The ID of the Docker container.
--
-- 'exitCode', 'container_exitCode' - The exit code returned from the container.
--
-- 'containerArn', 'container_containerArn' - The Amazon Resource Name (ARN) of the container.
--
-- 'name', 'container_name' - The name of the container.
--
-- 'image', 'container_image' - The image used for the container.
--
-- 'networkBindings', 'container_networkBindings' - The network bindings associated with the container.
--
-- 'reason', 'container_reason' - A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
--
-- 'cpu', 'container_cpu' - The number of CPU units set for the container. The value will be @0@ if
-- no value was specified in the container definition when the task
-- definition was registered.
--
-- 'lastStatus', 'container_lastStatus' - The last known status of the container.
--
-- 'taskArn', 'container_taskArn' - The ARN of the task.
--
-- 'healthStatus', 'container_healthStatus' - The health status of the container. If health checks are not configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
--
-- 'networkInterfaces', 'container_networkInterfaces' - The network interfaces associated with the container.
newContainer ::
  Container
newContainer =
  Container'
    { imageDigest = Core.Nothing,
      gpuIds = Core.Nothing,
      memory = Core.Nothing,
      memoryReservation = Core.Nothing,
      runtimeId = Core.Nothing,
      exitCode = Core.Nothing,
      containerArn = Core.Nothing,
      name = Core.Nothing,
      image = Core.Nothing,
      networkBindings = Core.Nothing,
      reason = Core.Nothing,
      cpu = Core.Nothing,
      lastStatus = Core.Nothing,
      taskArn = Core.Nothing,
      healthStatus = Core.Nothing,
      networkInterfaces = Core.Nothing
    }

-- | The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
container_imageDigest :: Lens.Lens' Container (Core.Maybe Core.Text)
container_imageDigest = Lens.lens (\Container' {imageDigest} -> imageDigest) (\s@Container' {} a -> s {imageDigest = a} :: Container)

-- | The IDs of each GPU assigned to the container.
container_gpuIds :: Lens.Lens' Container (Core.Maybe [Core.Text])
container_gpuIds = Lens.lens (\Container' {gpuIds} -> gpuIds) (\s@Container' {} a -> s {gpuIds = a} :: Container) Core.. Lens.mapping Lens._Coerce

-- | The hard limit (in MiB) of memory set for the container.
container_memory :: Lens.Lens' Container (Core.Maybe Core.Text)
container_memory = Lens.lens (\Container' {memory} -> memory) (\s@Container' {} a -> s {memory = a} :: Container)

-- | The soft limit (in MiB) of memory set for the container.
container_memoryReservation :: Lens.Lens' Container (Core.Maybe Core.Text)
container_memoryReservation = Lens.lens (\Container' {memoryReservation} -> memoryReservation) (\s@Container' {} a -> s {memoryReservation = a} :: Container)

-- | The ID of the Docker container.
container_runtimeId :: Lens.Lens' Container (Core.Maybe Core.Text)
container_runtimeId = Lens.lens (\Container' {runtimeId} -> runtimeId) (\s@Container' {} a -> s {runtimeId = a} :: Container)

-- | The exit code returned from the container.
container_exitCode :: Lens.Lens' Container (Core.Maybe Core.Int)
container_exitCode = Lens.lens (\Container' {exitCode} -> exitCode) (\s@Container' {} a -> s {exitCode = a} :: Container)

-- | The Amazon Resource Name (ARN) of the container.
container_containerArn :: Lens.Lens' Container (Core.Maybe Core.Text)
container_containerArn = Lens.lens (\Container' {containerArn} -> containerArn) (\s@Container' {} a -> s {containerArn = a} :: Container)

-- | The name of the container.
container_name :: Lens.Lens' Container (Core.Maybe Core.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | The image used for the container.
container_image :: Lens.Lens' Container (Core.Maybe Core.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | The network bindings associated with the container.
container_networkBindings :: Lens.Lens' Container (Core.Maybe [NetworkBinding])
container_networkBindings = Lens.lens (\Container' {networkBindings} -> networkBindings) (\s@Container' {} a -> s {networkBindings = a} :: Container) Core.. Lens.mapping Lens._Coerce

-- | A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
container_reason :: Lens.Lens' Container (Core.Maybe Core.Text)
container_reason = Lens.lens (\Container' {reason} -> reason) (\s@Container' {} a -> s {reason = a} :: Container)

-- | The number of CPU units set for the container. The value will be @0@ if
-- no value was specified in the container definition when the task
-- definition was registered.
container_cpu :: Lens.Lens' Container (Core.Maybe Core.Text)
container_cpu = Lens.lens (\Container' {cpu} -> cpu) (\s@Container' {} a -> s {cpu = a} :: Container)

-- | The last known status of the container.
container_lastStatus :: Lens.Lens' Container (Core.Maybe Core.Text)
container_lastStatus = Lens.lens (\Container' {lastStatus} -> lastStatus) (\s@Container' {} a -> s {lastStatus = a} :: Container)

-- | The ARN of the task.
container_taskArn :: Lens.Lens' Container (Core.Maybe Core.Text)
container_taskArn = Lens.lens (\Container' {taskArn} -> taskArn) (\s@Container' {} a -> s {taskArn = a} :: Container)

-- | The health status of the container. If health checks are not configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
container_healthStatus :: Lens.Lens' Container (Core.Maybe HealthStatus)
container_healthStatus = Lens.lens (\Container' {healthStatus} -> healthStatus) (\s@Container' {} a -> s {healthStatus = a} :: Container)

-- | The network interfaces associated with the container.
container_networkInterfaces :: Lens.Lens' Container (Core.Maybe [NetworkInterface])
container_networkInterfaces = Lens.lens (\Container' {networkInterfaces} -> networkInterfaces) (\s@Container' {} a -> s {networkInterfaces = a} :: Container) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject
      "Container"
      ( \x ->
          Container'
            Core.<$> (x Core..:? "imageDigest")
            Core.<*> (x Core..:? "gpuIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "memory")
            Core.<*> (x Core..:? "memoryReservation")
            Core.<*> (x Core..:? "runtimeId")
            Core.<*> (x Core..:? "exitCode")
            Core.<*> (x Core..:? "containerArn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "image")
            Core.<*> (x Core..:? "networkBindings" Core..!= Core.mempty)
            Core.<*> (x Core..:? "reason")
            Core.<*> (x Core..:? "cpu")
            Core.<*> (x Core..:? "lastStatus")
            Core.<*> (x Core..:? "taskArn")
            Core.<*> (x Core..:? "healthStatus")
            Core.<*> ( x Core..:? "networkInterfaces"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable Container

instance Core.NFData Container
