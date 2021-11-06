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
-- Module      : Amazonka.ECS.Types.Container
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Container where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.HealthStatus
import Amazonka.ECS.Types.ManagedAgent
import Amazonka.ECS.Types.NetworkBinding
import Amazonka.ECS.Types.NetworkInterface
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A Docker container that is part of a task.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The IDs of each GPU assigned to the container.
    gpuIds :: Prelude.Maybe [Prelude.Text],
    -- | The network bindings associated with the container.
    networkBindings :: Prelude.Maybe [NetworkBinding],
    -- | The details of any Amazon ECS managed agents associated with the
    -- container.
    managedAgents :: Prelude.Maybe [ManagedAgent],
    -- | The image used for the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the container.
    containerArn :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces associated with the container.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The ARN of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The last known status of the container.
    lastStatus :: Prelude.Maybe Prelude.Text,
    -- | The hard limit (in MiB) of memory set for the container.
    memory :: Prelude.Maybe Prelude.Text,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the container.
    name :: Prelude.Maybe Prelude.Text,
    -- | The container image manifest digest.
    --
    -- The @imageDigest@ is only returned if the container is using an image
    -- hosted in Amazon ECR, otherwise it is omitted.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The exit code returned from the container.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The health status of the container. If health checks are not configured
    -- for this container in its task definition, then it reports the health
    -- status as @UNKNOWN@.
    healthStatus :: Prelude.Maybe HealthStatus,
    -- | The number of CPU units set for the container. The value will be @0@ if
    -- no value was specified in the container definition when the task
    -- definition was registered.
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Docker container.
    runtimeId :: Prelude.Maybe Prelude.Text,
    -- | The soft limit (in MiB) of memory set for the container.
    memoryReservation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gpuIds', 'container_gpuIds' - The IDs of each GPU assigned to the container.
--
-- 'networkBindings', 'container_networkBindings' - The network bindings associated with the container.
--
-- 'managedAgents', 'container_managedAgents' - The details of any Amazon ECS managed agents associated with the
-- container.
--
-- 'image', 'container_image' - The image used for the container.
--
-- 'containerArn', 'container_containerArn' - The Amazon Resource Name (ARN) of the container.
--
-- 'networkInterfaces', 'container_networkInterfaces' - The network interfaces associated with the container.
--
-- 'taskArn', 'container_taskArn' - The ARN of the task.
--
-- 'lastStatus', 'container_lastStatus' - The last known status of the container.
--
-- 'memory', 'container_memory' - The hard limit (in MiB) of memory set for the container.
--
-- 'reason', 'container_reason' - A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
--
-- 'name', 'container_name' - The name of the container.
--
-- 'imageDigest', 'container_imageDigest' - The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
--
-- 'exitCode', 'container_exitCode' - The exit code returned from the container.
--
-- 'healthStatus', 'container_healthStatus' - The health status of the container. If health checks are not configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
--
-- 'cpu', 'container_cpu' - The number of CPU units set for the container. The value will be @0@ if
-- no value was specified in the container definition when the task
-- definition was registered.
--
-- 'runtimeId', 'container_runtimeId' - The ID of the Docker container.
--
-- 'memoryReservation', 'container_memoryReservation' - The soft limit (in MiB) of memory set for the container.
newContainer ::
  Container
newContainer =
  Container'
    { gpuIds = Prelude.Nothing,
      networkBindings = Prelude.Nothing,
      managedAgents = Prelude.Nothing,
      image = Prelude.Nothing,
      containerArn = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      memory = Prelude.Nothing,
      reason = Prelude.Nothing,
      name = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      cpu = Prelude.Nothing,
      runtimeId = Prelude.Nothing,
      memoryReservation = Prelude.Nothing
    }

-- | The IDs of each GPU assigned to the container.
container_gpuIds :: Lens.Lens' Container (Prelude.Maybe [Prelude.Text])
container_gpuIds = Lens.lens (\Container' {gpuIds} -> gpuIds) (\s@Container' {} a -> s {gpuIds = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The network bindings associated with the container.
container_networkBindings :: Lens.Lens' Container (Prelude.Maybe [NetworkBinding])
container_networkBindings = Lens.lens (\Container' {networkBindings} -> networkBindings) (\s@Container' {} a -> s {networkBindings = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The details of any Amazon ECS managed agents associated with the
-- container.
container_managedAgents :: Lens.Lens' Container (Prelude.Maybe [ManagedAgent])
container_managedAgents = Lens.lens (\Container' {managedAgents} -> managedAgents) (\s@Container' {} a -> s {managedAgents = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The image used for the container.
container_image :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | The Amazon Resource Name (ARN) of the container.
container_containerArn :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_containerArn = Lens.lens (\Container' {containerArn} -> containerArn) (\s@Container' {} a -> s {containerArn = a} :: Container)

-- | The network interfaces associated with the container.
container_networkInterfaces :: Lens.Lens' Container (Prelude.Maybe [NetworkInterface])
container_networkInterfaces = Lens.lens (\Container' {networkInterfaces} -> networkInterfaces) (\s@Container' {} a -> s {networkInterfaces = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the task.
container_taskArn :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_taskArn = Lens.lens (\Container' {taskArn} -> taskArn) (\s@Container' {} a -> s {taskArn = a} :: Container)

-- | The last known status of the container.
container_lastStatus :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_lastStatus = Lens.lens (\Container' {lastStatus} -> lastStatus) (\s@Container' {} a -> s {lastStatus = a} :: Container)

-- | The hard limit (in MiB) of memory set for the container.
container_memory :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_memory = Lens.lens (\Container' {memory} -> memory) (\s@Container' {} a -> s {memory = a} :: Container)

-- | A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
container_reason :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_reason = Lens.lens (\Container' {reason} -> reason) (\s@Container' {} a -> s {reason = a} :: Container)

-- | The name of the container.
container_name :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
container_imageDigest :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_imageDigest = Lens.lens (\Container' {imageDigest} -> imageDigest) (\s@Container' {} a -> s {imageDigest = a} :: Container)

-- | The exit code returned from the container.
container_exitCode :: Lens.Lens' Container (Prelude.Maybe Prelude.Int)
container_exitCode = Lens.lens (\Container' {exitCode} -> exitCode) (\s@Container' {} a -> s {exitCode = a} :: Container)

-- | The health status of the container. If health checks are not configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
container_healthStatus :: Lens.Lens' Container (Prelude.Maybe HealthStatus)
container_healthStatus = Lens.lens (\Container' {healthStatus} -> healthStatus) (\s@Container' {} a -> s {healthStatus = a} :: Container)

-- | The number of CPU units set for the container. The value will be @0@ if
-- no value was specified in the container definition when the task
-- definition was registered.
container_cpu :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_cpu = Lens.lens (\Container' {cpu} -> cpu) (\s@Container' {} a -> s {cpu = a} :: Container)

-- | The ID of the Docker container.
container_runtimeId :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_runtimeId = Lens.lens (\Container' {runtimeId} -> runtimeId) (\s@Container' {} a -> s {runtimeId = a} :: Container)

-- | The soft limit (in MiB) of memory set for the container.
container_memoryReservation :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_memoryReservation = Lens.lens (\Container' {memoryReservation} -> memoryReservation) (\s@Container' {} a -> s {memoryReservation = a} :: Container)

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Core..:? "gpuIds" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "networkBindings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "managedAgents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "image")
            Prelude.<*> (x Core..:? "containerArn")
            Prelude.<*> ( x Core..:? "networkInterfaces"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "taskArn")
            Prelude.<*> (x Core..:? "lastStatus")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "imageDigest")
            Prelude.<*> (x Core..:? "exitCode")
            Prelude.<*> (x Core..:? "healthStatus")
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> (x Core..:? "runtimeId")
            Prelude.<*> (x Core..:? "memoryReservation")
      )

instance Prelude.Hashable Container

instance Prelude.NFData Container
