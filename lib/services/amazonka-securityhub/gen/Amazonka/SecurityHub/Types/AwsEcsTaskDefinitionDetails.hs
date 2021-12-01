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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails

-- | details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
--
-- /See:/ 'newAwsEcsTaskDefinitionDetails' smart constructor.
data AwsEcsTaskDefinitionDetails = AwsEcsTaskDefinitionDetails'
  { -- | The Elastic Inference accelerators to use for the containers in the
    -- task.
    inferenceAccelerators :: Prelude.Maybe [AwsEcsTaskDefinitionInferenceAcceleratorsDetails],
    -- | The ARN of the task execution role that grants the container agent
    -- permission to make API calls on behalf of the container user.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The process namespace to use for the containers in the task.
    pidMode :: Prelude.Maybe Prelude.Text,
    -- | The name of a family that this task definition is registered to.
    family :: Prelude.Maybe Prelude.Text,
    -- | The task launch types that the task definition was validated against.
    requiresCompatibilities :: Prelude.Maybe [Prelude.Text],
    -- | The container definitions that describe the containers that make up the
    -- task.
    containerDefinitions :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDetails],
    -- | The amount (in MiB) of memory used by the task.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The IPC resource namespace to use for the containers in the task.
    ipcMode :: Prelude.Maybe Prelude.Text,
    -- | The short name or ARN of the IAM role that grants containers in the task
    -- permission to call Amazon Web Services API operations on your behalf.
    taskRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration details for the App Mesh proxy.
    proxyConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionProxyConfigurationDetails,
    -- | The placement constraint objects to use for tasks.
    placementConstraints :: Prelude.Maybe [AwsEcsTaskDefinitionPlacementConstraintsDetails],
    -- | The Docker networking mode to use for the containers in the task.
    networkMode :: Prelude.Maybe Prelude.Text,
    -- | The data volume definitions for the task.
    volumes :: Prelude.Maybe [AwsEcsTaskDefinitionVolumesDetails],
    -- | The number of CPU units used by the task.
    cpu :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceAccelerators', 'awsEcsTaskDefinitionDetails_inferenceAccelerators' - The Elastic Inference accelerators to use for the containers in the
-- task.
--
-- 'executionRoleArn', 'awsEcsTaskDefinitionDetails_executionRoleArn' - The ARN of the task execution role that grants the container agent
-- permission to make API calls on behalf of the container user.
--
-- 'pidMode', 'awsEcsTaskDefinitionDetails_pidMode' - The process namespace to use for the containers in the task.
--
-- 'family', 'awsEcsTaskDefinitionDetails_family' - The name of a family that this task definition is registered to.
--
-- 'requiresCompatibilities', 'awsEcsTaskDefinitionDetails_requiresCompatibilities' - The task launch types that the task definition was validated against.
--
-- 'containerDefinitions', 'awsEcsTaskDefinitionDetails_containerDefinitions' - The container definitions that describe the containers that make up the
-- task.
--
-- 'memory', 'awsEcsTaskDefinitionDetails_memory' - The amount (in MiB) of memory used by the task.
--
-- 'ipcMode', 'awsEcsTaskDefinitionDetails_ipcMode' - The IPC resource namespace to use for the containers in the task.
--
-- 'taskRoleArn', 'awsEcsTaskDefinitionDetails_taskRoleArn' - The short name or ARN of the IAM role that grants containers in the task
-- permission to call Amazon Web Services API operations on your behalf.
--
-- 'proxyConfiguration', 'awsEcsTaskDefinitionDetails_proxyConfiguration' - The configuration details for the App Mesh proxy.
--
-- 'placementConstraints', 'awsEcsTaskDefinitionDetails_placementConstraints' - The placement constraint objects to use for tasks.
--
-- 'networkMode', 'awsEcsTaskDefinitionDetails_networkMode' - The Docker networking mode to use for the containers in the task.
--
-- 'volumes', 'awsEcsTaskDefinitionDetails_volumes' - The data volume definitions for the task.
--
-- 'cpu', 'awsEcsTaskDefinitionDetails_cpu' - The number of CPU units used by the task.
newAwsEcsTaskDefinitionDetails ::
  AwsEcsTaskDefinitionDetails
newAwsEcsTaskDefinitionDetails =
  AwsEcsTaskDefinitionDetails'
    { inferenceAccelerators =
        Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      pidMode = Prelude.Nothing,
      family = Prelude.Nothing,
      requiresCompatibilities = Prelude.Nothing,
      containerDefinitions = Prelude.Nothing,
      memory = Prelude.Nothing,
      ipcMode = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing,
      proxyConfiguration = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      networkMode = Prelude.Nothing,
      volumes = Prelude.Nothing,
      cpu = Prelude.Nothing
    }

-- | The Elastic Inference accelerators to use for the containers in the
-- task.
awsEcsTaskDefinitionDetails_inferenceAccelerators :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionInferenceAcceleratorsDetails])
awsEcsTaskDefinitionDetails_inferenceAccelerators = Lens.lens (\AwsEcsTaskDefinitionDetails' {inferenceAccelerators} -> inferenceAccelerators) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {inferenceAccelerators = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the task execution role that grants the container agent
-- permission to make API calls on behalf of the container user.
awsEcsTaskDefinitionDetails_executionRoleArn :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_executionRoleArn = Lens.lens (\AwsEcsTaskDefinitionDetails' {executionRoleArn} -> executionRoleArn) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {executionRoleArn = a} :: AwsEcsTaskDefinitionDetails)

-- | The process namespace to use for the containers in the task.
awsEcsTaskDefinitionDetails_pidMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_pidMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {pidMode} -> pidMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {pidMode = a} :: AwsEcsTaskDefinitionDetails)

-- | The name of a family that this task definition is registered to.
awsEcsTaskDefinitionDetails_family :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_family = Lens.lens (\AwsEcsTaskDefinitionDetails' {family} -> family) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {family = a} :: AwsEcsTaskDefinitionDetails)

-- | The task launch types that the task definition was validated against.
awsEcsTaskDefinitionDetails_requiresCompatibilities :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionDetails_requiresCompatibilities = Lens.lens (\AwsEcsTaskDefinitionDetails' {requiresCompatibilities} -> requiresCompatibilities) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {requiresCompatibilities = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The container definitions that describe the containers that make up the
-- task.
awsEcsTaskDefinitionDetails_containerDefinitions :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDetails])
awsEcsTaskDefinitionDetails_containerDefinitions = Lens.lens (\AwsEcsTaskDefinitionDetails' {containerDefinitions} -> containerDefinitions) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {containerDefinitions = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The amount (in MiB) of memory used by the task.
awsEcsTaskDefinitionDetails_memory :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_memory = Lens.lens (\AwsEcsTaskDefinitionDetails' {memory} -> memory) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {memory = a} :: AwsEcsTaskDefinitionDetails)

-- | The IPC resource namespace to use for the containers in the task.
awsEcsTaskDefinitionDetails_ipcMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_ipcMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {ipcMode} -> ipcMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {ipcMode = a} :: AwsEcsTaskDefinitionDetails)

-- | The short name or ARN of the IAM role that grants containers in the task
-- permission to call Amazon Web Services API operations on your behalf.
awsEcsTaskDefinitionDetails_taskRoleArn :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_taskRoleArn = Lens.lens (\AwsEcsTaskDefinitionDetails' {taskRoleArn} -> taskRoleArn) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {taskRoleArn = a} :: AwsEcsTaskDefinitionDetails)

-- | The configuration details for the App Mesh proxy.
awsEcsTaskDefinitionDetails_proxyConfiguration :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe AwsEcsTaskDefinitionProxyConfigurationDetails)
awsEcsTaskDefinitionDetails_proxyConfiguration = Lens.lens (\AwsEcsTaskDefinitionDetails' {proxyConfiguration} -> proxyConfiguration) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {proxyConfiguration = a} :: AwsEcsTaskDefinitionDetails)

-- | The placement constraint objects to use for tasks.
awsEcsTaskDefinitionDetails_placementConstraints :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionPlacementConstraintsDetails])
awsEcsTaskDefinitionDetails_placementConstraints = Lens.lens (\AwsEcsTaskDefinitionDetails' {placementConstraints} -> placementConstraints) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {placementConstraints = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Docker networking mode to use for the containers in the task.
awsEcsTaskDefinitionDetails_networkMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_networkMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {networkMode} -> networkMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {networkMode = a} :: AwsEcsTaskDefinitionDetails)

-- | The data volume definitions for the task.
awsEcsTaskDefinitionDetails_volumes :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionVolumesDetails])
awsEcsTaskDefinitionDetails_volumes = Lens.lens (\AwsEcsTaskDefinitionDetails' {volumes} -> volumes) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {volumes = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of CPU units used by the task.
awsEcsTaskDefinitionDetails_cpu :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_cpu = Lens.lens (\AwsEcsTaskDefinitionDetails' {cpu} -> cpu) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {cpu = a} :: AwsEcsTaskDefinitionDetails)

instance Core.FromJSON AwsEcsTaskDefinitionDetails where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionDetails"
      ( \x ->
          AwsEcsTaskDefinitionDetails'
            Prelude.<$> ( x Core..:? "InferenceAccelerators"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ExecutionRoleArn")
            Prelude.<*> (x Core..:? "PidMode")
            Prelude.<*> (x Core..:? "Family")
            Prelude.<*> ( x Core..:? "RequiresCompatibilities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ContainerDefinitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Memory")
            Prelude.<*> (x Core..:? "IpcMode")
            Prelude.<*> (x Core..:? "TaskRoleArn")
            Prelude.<*> (x Core..:? "ProxyConfiguration")
            Prelude.<*> ( x Core..:? "PlacementConstraints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NetworkMode")
            Prelude.<*> (x Core..:? "Volumes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Cpu")
      )

instance Prelude.Hashable AwsEcsTaskDefinitionDetails where
  hashWithSalt salt' AwsEcsTaskDefinitionDetails' {..} =
    salt' `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` networkMode
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` taskRoleArn
      `Prelude.hashWithSalt` ipcMode
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` containerDefinitions
      `Prelude.hashWithSalt` requiresCompatibilities
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` pidMode
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inferenceAccelerators

instance Prelude.NFData AwsEcsTaskDefinitionDetails where
  rnf AwsEcsTaskDefinitionDetails' {..} =
    Prelude.rnf inferenceAccelerators
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf networkMode
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf taskRoleArn
      `Prelude.seq` Prelude.rnf ipcMode
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf containerDefinitions
      `Prelude.seq` Prelude.rnf requiresCompatibilities
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf pidMode
      `Prelude.seq` Prelude.rnf executionRoleArn

instance Core.ToJSON AwsEcsTaskDefinitionDetails where
  toJSON AwsEcsTaskDefinitionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InferenceAccelerators" Core..=)
              Prelude.<$> inferenceAccelerators,
            ("ExecutionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            ("PidMode" Core..=) Prelude.<$> pidMode,
            ("Family" Core..=) Prelude.<$> family,
            ("RequiresCompatibilities" Core..=)
              Prelude.<$> requiresCompatibilities,
            ("ContainerDefinitions" Core..=)
              Prelude.<$> containerDefinitions,
            ("Memory" Core..=) Prelude.<$> memory,
            ("IpcMode" Core..=) Prelude.<$> ipcMode,
            ("TaskRoleArn" Core..=) Prelude.<$> taskRoleArn,
            ("ProxyConfiguration" Core..=)
              Prelude.<$> proxyConfiguration,
            ("PlacementConstraints" Core..=)
              Prelude.<$> placementConstraints,
            ("NetworkMode" Core..=) Prelude.<$> networkMode,
            ("Volumes" Core..=) Prelude.<$> volumes,
            ("Cpu" Core..=) Prelude.<$> cpu
          ]
      )
