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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.HealthStatus
import Amazonka.ECS.Types.ManagedAgent
import Amazonka.ECS.Types.NetworkBinding
import Amazonka.ECS.Types.NetworkInterface
import qualified Amazonka.Prelude as Prelude

-- | A Docker container that\'s part of a task.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The name of the container.
    name :: Prelude.Maybe Prelude.Text,
    -- | The hard limit (in MiB) of memory set for the container.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU units set for the container. The value is @0@ if no
    -- value was specified in the container definition when the task definition
    -- was registered.
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The soft limit (in MiB) of memory set for the container.
    memoryReservation :: Prelude.Maybe Prelude.Text,
    -- | The health status of the container. If health checks aren\'t configured
    -- for this container in its task definition, then it reports the health
    -- status as @UNKNOWN@.
    healthStatus :: Prelude.Maybe HealthStatus,
    -- | The last known status of the container.
    lastStatus :: Prelude.Maybe Prelude.Text,
    -- | The IDs of each GPU assigned to the container.
    gpuIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Docker container.
    runtimeId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the container.
    containerArn :: Prelude.Maybe Prelude.Text,
    -- | The details of any Amazon ECS managed agents associated with the
    -- container.
    managedAgents :: Prelude.Maybe [ManagedAgent],
    -- | The network bindings associated with the container.
    networkBindings :: Prelude.Maybe [NetworkBinding],
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The exit code returned from the container.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The container image manifest digest.
    --
    -- The @imageDigest@ is only returned if the container is using an image
    -- hosted in Amazon ECR, otherwise it is omitted.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The image used for the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces associated with the container.
    networkInterfaces :: Prelude.Maybe [NetworkInterface]
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
-- 'name', 'container_name' - The name of the container.
--
-- 'memory', 'container_memory' - The hard limit (in MiB) of memory set for the container.
--
-- 'cpu', 'container_cpu' - The number of CPU units set for the container. The value is @0@ if no
-- value was specified in the container definition when the task definition
-- was registered.
--
-- 'taskArn', 'container_taskArn' - The ARN of the task.
--
-- 'memoryReservation', 'container_memoryReservation' - The soft limit (in MiB) of memory set for the container.
--
-- 'healthStatus', 'container_healthStatus' - The health status of the container. If health checks aren\'t configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
--
-- 'lastStatus', 'container_lastStatus' - The last known status of the container.
--
-- 'gpuIds', 'container_gpuIds' - The IDs of each GPU assigned to the container.
--
-- 'runtimeId', 'container_runtimeId' - The ID of the Docker container.
--
-- 'containerArn', 'container_containerArn' - The Amazon Resource Name (ARN) of the container.
--
-- 'managedAgents', 'container_managedAgents' - The details of any Amazon ECS managed agents associated with the
-- container.
--
-- 'networkBindings', 'container_networkBindings' - The network bindings associated with the container.
--
-- 'reason', 'container_reason' - A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
--
-- 'exitCode', 'container_exitCode' - The exit code returned from the container.
--
-- 'imageDigest', 'container_imageDigest' - The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
--
-- 'image', 'container_image' - The image used for the container.
--
-- 'networkInterfaces', 'container_networkInterfaces' - The network interfaces associated with the container.
newContainer ::
  Container
newContainer =
  Container'
    { name = Prelude.Nothing,
      memory = Prelude.Nothing,
      cpu = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      memoryReservation = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      gpuIds = Prelude.Nothing,
      runtimeId = Prelude.Nothing,
      containerArn = Prelude.Nothing,
      managedAgents = Prelude.Nothing,
      networkBindings = Prelude.Nothing,
      reason = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      image = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | The name of the container.
container_name :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | The hard limit (in MiB) of memory set for the container.
container_memory :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_memory = Lens.lens (\Container' {memory} -> memory) (\s@Container' {} a -> s {memory = a} :: Container)

-- | The number of CPU units set for the container. The value is @0@ if no
-- value was specified in the container definition when the task definition
-- was registered.
container_cpu :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_cpu = Lens.lens (\Container' {cpu} -> cpu) (\s@Container' {} a -> s {cpu = a} :: Container)

-- | The ARN of the task.
container_taskArn :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_taskArn = Lens.lens (\Container' {taskArn} -> taskArn) (\s@Container' {} a -> s {taskArn = a} :: Container)

-- | The soft limit (in MiB) of memory set for the container.
container_memoryReservation :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_memoryReservation = Lens.lens (\Container' {memoryReservation} -> memoryReservation) (\s@Container' {} a -> s {memoryReservation = a} :: Container)

-- | The health status of the container. If health checks aren\'t configured
-- for this container in its task definition, then it reports the health
-- status as @UNKNOWN@.
container_healthStatus :: Lens.Lens' Container (Prelude.Maybe HealthStatus)
container_healthStatus = Lens.lens (\Container' {healthStatus} -> healthStatus) (\s@Container' {} a -> s {healthStatus = a} :: Container)

-- | The last known status of the container.
container_lastStatus :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_lastStatus = Lens.lens (\Container' {lastStatus} -> lastStatus) (\s@Container' {} a -> s {lastStatus = a} :: Container)

-- | The IDs of each GPU assigned to the container.
container_gpuIds :: Lens.Lens' Container (Prelude.Maybe [Prelude.Text])
container_gpuIds = Lens.lens (\Container' {gpuIds} -> gpuIds) (\s@Container' {} a -> s {gpuIds = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Docker container.
container_runtimeId :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_runtimeId = Lens.lens (\Container' {runtimeId} -> runtimeId) (\s@Container' {} a -> s {runtimeId = a} :: Container)

-- | The Amazon Resource Name (ARN) of the container.
container_containerArn :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_containerArn = Lens.lens (\Container' {containerArn} -> containerArn) (\s@Container' {} a -> s {containerArn = a} :: Container)

-- | The details of any Amazon ECS managed agents associated with the
-- container.
container_managedAgents :: Lens.Lens' Container (Prelude.Maybe [ManagedAgent])
container_managedAgents = Lens.lens (\Container' {managedAgents} -> managedAgents) (\s@Container' {} a -> s {managedAgents = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The network bindings associated with the container.
container_networkBindings :: Lens.Lens' Container (Prelude.Maybe [NetworkBinding])
container_networkBindings = Lens.lens (\Container' {networkBindings} -> networkBindings) (\s@Container' {} a -> s {networkBindings = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
container_reason :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_reason = Lens.lens (\Container' {reason} -> reason) (\s@Container' {} a -> s {reason = a} :: Container)

-- | The exit code returned from the container.
container_exitCode :: Lens.Lens' Container (Prelude.Maybe Prelude.Int)
container_exitCode = Lens.lens (\Container' {exitCode} -> exitCode) (\s@Container' {} a -> s {exitCode = a} :: Container)

-- | The container image manifest digest.
--
-- The @imageDigest@ is only returned if the container is using an image
-- hosted in Amazon ECR, otherwise it is omitted.
container_imageDigest :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_imageDigest = Lens.lens (\Container' {imageDigest} -> imageDigest) (\s@Container' {} a -> s {imageDigest = a} :: Container)

-- | The image used for the container.
container_image :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | The network interfaces associated with the container.
container_networkInterfaces :: Lens.Lens' Container (Prelude.Maybe [NetworkInterface])
container_networkInterfaces = Lens.lens (\Container' {networkInterfaces} -> networkInterfaces) (\s@Container' {} a -> s {networkInterfaces = a} :: Container) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Container where
  parseJSON =
    Data.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "taskArn")
            Prelude.<*> (x Data..:? "memoryReservation")
            Prelude.<*> (x Data..:? "healthStatus")
            Prelude.<*> (x Data..:? "lastStatus")
            Prelude.<*> (x Data..:? "gpuIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "runtimeId")
            Prelude.<*> (x Data..:? "containerArn")
            Prelude.<*> (x Data..:? "managedAgents" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "networkBindings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "imageDigest")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> ( x Data..:? "networkInterfaces"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` memoryReservation
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` gpuIds
      `Prelude.hashWithSalt` runtimeId
      `Prelude.hashWithSalt` containerArn
      `Prelude.hashWithSalt` managedAgents
      `Prelude.hashWithSalt` networkBindings
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` imageDigest
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` networkInterfaces

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf memoryReservation
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf gpuIds
      `Prelude.seq` Prelude.rnf runtimeId
      `Prelude.seq` Prelude.rnf containerArn
      `Prelude.seq` Prelude.rnf managedAgents
      `Prelude.seq` Prelude.rnf networkBindings
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf networkInterfaces
