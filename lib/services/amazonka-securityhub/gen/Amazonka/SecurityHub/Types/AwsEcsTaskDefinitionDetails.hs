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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails

-- | Details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
--
-- /See:/ 'newAwsEcsTaskDefinitionDetails' smart constructor.
data AwsEcsTaskDefinitionDetails = AwsEcsTaskDefinitionDetails'
  { -- | The configuration details for the App Mesh proxy.
    proxyConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionProxyConfigurationDetails,
    -- | The process namespace to use for the containers in the task. Valid
    -- values are @host@ or @task@.
    pidMode :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU units used by the task.Valid values are as follows:
    --
    -- -   @256 (.25 vCPU)@
    --
    -- -   @512 (.5 vCPU)@
    --
    -- -   @1024 (1 vCPU)@
    --
    -- -   @2048 (2 vCPU)@
    --
    -- -   @4096 (4 vCPU)@
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The amount (in MiB) of memory used by the task.
    --
    -- For tasks that are hosted on Amazon EC2, you can provide a task-level
    -- memory value or a container-level memory value. For tasks that are
    -- hosted on Fargate, you must use one of the
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html#task_size specified values>
    -- in the //Amazon Elastic Container Service Developer Guide// , which
    -- determines your range of supported values for the @Cpu@ and @Memory@
    -- parameters.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The short name or ARN of the IAM role that grants containers in the task
    -- permission to call Amazon Web Services API operations on your behalf.
    taskRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerators to use for the containers in the
    -- task.
    inferenceAccelerators :: Prelude.Maybe [AwsEcsTaskDefinitionInferenceAcceleratorsDetails],
    -- | The data volume definitions for the task.
    volumes :: Prelude.Maybe [AwsEcsTaskDefinitionVolumesDetails],
    -- | The task launch types that the task definition was validated against.
    requiresCompatibilities :: Prelude.Maybe [Prelude.Text],
    -- | The placement constraint objects to use for tasks.
    placementConstraints :: Prelude.Maybe [AwsEcsTaskDefinitionPlacementConstraintsDetails],
    -- | The name of a family that this task definition is registered to.
    family :: Prelude.Maybe Prelude.Text,
    -- | The container definitions that describe the containers that make up the
    -- task.
    containerDefinitions :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDetails],
    -- | The ARN of the task execution role that grants the container agent
    -- permission to make API calls on behalf of the container user.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Docker networking mode to use for the containers in the task. Valid
    -- values are as follows:
    --
    -- -   @awsvpc@
    --
    -- -   @bridge@
    --
    -- -   @host@
    --
    -- -   @none@
    networkMode :: Prelude.Maybe Prelude.Text,
    -- | The inter-process communication (IPC) resource namespace to use for the
    -- containers in the task. Valid values are as follows:
    --
    -- -   @host@
    --
    -- -   @none@
    --
    -- -   @task@
    ipcMode :: Prelude.Maybe Prelude.Text
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
-- 'proxyConfiguration', 'awsEcsTaskDefinitionDetails_proxyConfiguration' - The configuration details for the App Mesh proxy.
--
-- 'pidMode', 'awsEcsTaskDefinitionDetails_pidMode' - The process namespace to use for the containers in the task. Valid
-- values are @host@ or @task@.
--
-- 'cpu', 'awsEcsTaskDefinitionDetails_cpu' - The number of CPU units used by the task.Valid values are as follows:
--
-- -   @256 (.25 vCPU)@
--
-- -   @512 (.5 vCPU)@
--
-- -   @1024 (1 vCPU)@
--
-- -   @2048 (2 vCPU)@
--
-- -   @4096 (4 vCPU)@
--
-- 'memory', 'awsEcsTaskDefinitionDetails_memory' - The amount (in MiB) of memory used by the task.
--
-- For tasks that are hosted on Amazon EC2, you can provide a task-level
-- memory value or a container-level memory value. For tasks that are
-- hosted on Fargate, you must use one of the
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html#task_size specified values>
-- in the //Amazon Elastic Container Service Developer Guide// , which
-- determines your range of supported values for the @Cpu@ and @Memory@
-- parameters.
--
-- 'taskRoleArn', 'awsEcsTaskDefinitionDetails_taskRoleArn' - The short name or ARN of the IAM role that grants containers in the task
-- permission to call Amazon Web Services API operations on your behalf.
--
-- 'inferenceAccelerators', 'awsEcsTaskDefinitionDetails_inferenceAccelerators' - The Elastic Inference accelerators to use for the containers in the
-- task.
--
-- 'volumes', 'awsEcsTaskDefinitionDetails_volumes' - The data volume definitions for the task.
--
-- 'requiresCompatibilities', 'awsEcsTaskDefinitionDetails_requiresCompatibilities' - The task launch types that the task definition was validated against.
--
-- 'placementConstraints', 'awsEcsTaskDefinitionDetails_placementConstraints' - The placement constraint objects to use for tasks.
--
-- 'family', 'awsEcsTaskDefinitionDetails_family' - The name of a family that this task definition is registered to.
--
-- 'containerDefinitions', 'awsEcsTaskDefinitionDetails_containerDefinitions' - The container definitions that describe the containers that make up the
-- task.
--
-- 'executionRoleArn', 'awsEcsTaskDefinitionDetails_executionRoleArn' - The ARN of the task execution role that grants the container agent
-- permission to make API calls on behalf of the container user.
--
-- 'networkMode', 'awsEcsTaskDefinitionDetails_networkMode' - The Docker networking mode to use for the containers in the task. Valid
-- values are as follows:
--
-- -   @awsvpc@
--
-- -   @bridge@
--
-- -   @host@
--
-- -   @none@
--
-- 'ipcMode', 'awsEcsTaskDefinitionDetails_ipcMode' - The inter-process communication (IPC) resource namespace to use for the
-- containers in the task. Valid values are as follows:
--
-- -   @host@
--
-- -   @none@
--
-- -   @task@
newAwsEcsTaskDefinitionDetails ::
  AwsEcsTaskDefinitionDetails
newAwsEcsTaskDefinitionDetails =
  AwsEcsTaskDefinitionDetails'
    { proxyConfiguration =
        Prelude.Nothing,
      pidMode = Prelude.Nothing,
      cpu = Prelude.Nothing,
      memory = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing,
      inferenceAccelerators = Prelude.Nothing,
      volumes = Prelude.Nothing,
      requiresCompatibilities = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      family = Prelude.Nothing,
      containerDefinitions = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      networkMode = Prelude.Nothing,
      ipcMode = Prelude.Nothing
    }

-- | The configuration details for the App Mesh proxy.
awsEcsTaskDefinitionDetails_proxyConfiguration :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe AwsEcsTaskDefinitionProxyConfigurationDetails)
awsEcsTaskDefinitionDetails_proxyConfiguration = Lens.lens (\AwsEcsTaskDefinitionDetails' {proxyConfiguration} -> proxyConfiguration) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {proxyConfiguration = a} :: AwsEcsTaskDefinitionDetails)

-- | The process namespace to use for the containers in the task. Valid
-- values are @host@ or @task@.
awsEcsTaskDefinitionDetails_pidMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_pidMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {pidMode} -> pidMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {pidMode = a} :: AwsEcsTaskDefinitionDetails)

-- | The number of CPU units used by the task.Valid values are as follows:
--
-- -   @256 (.25 vCPU)@
--
-- -   @512 (.5 vCPU)@
--
-- -   @1024 (1 vCPU)@
--
-- -   @2048 (2 vCPU)@
--
-- -   @4096 (4 vCPU)@
awsEcsTaskDefinitionDetails_cpu :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_cpu = Lens.lens (\AwsEcsTaskDefinitionDetails' {cpu} -> cpu) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {cpu = a} :: AwsEcsTaskDefinitionDetails)

-- | The amount (in MiB) of memory used by the task.
--
-- For tasks that are hosted on Amazon EC2, you can provide a task-level
-- memory value or a container-level memory value. For tasks that are
-- hosted on Fargate, you must use one of the
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html#task_size specified values>
-- in the //Amazon Elastic Container Service Developer Guide// , which
-- determines your range of supported values for the @Cpu@ and @Memory@
-- parameters.
awsEcsTaskDefinitionDetails_memory :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_memory = Lens.lens (\AwsEcsTaskDefinitionDetails' {memory} -> memory) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {memory = a} :: AwsEcsTaskDefinitionDetails)

-- | The short name or ARN of the IAM role that grants containers in the task
-- permission to call Amazon Web Services API operations on your behalf.
awsEcsTaskDefinitionDetails_taskRoleArn :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_taskRoleArn = Lens.lens (\AwsEcsTaskDefinitionDetails' {taskRoleArn} -> taskRoleArn) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {taskRoleArn = a} :: AwsEcsTaskDefinitionDetails)

-- | The Elastic Inference accelerators to use for the containers in the
-- task.
awsEcsTaskDefinitionDetails_inferenceAccelerators :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionInferenceAcceleratorsDetails])
awsEcsTaskDefinitionDetails_inferenceAccelerators = Lens.lens (\AwsEcsTaskDefinitionDetails' {inferenceAccelerators} -> inferenceAccelerators) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {inferenceAccelerators = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The data volume definitions for the task.
awsEcsTaskDefinitionDetails_volumes :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionVolumesDetails])
awsEcsTaskDefinitionDetails_volumes = Lens.lens (\AwsEcsTaskDefinitionDetails' {volumes} -> volumes) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {volumes = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The task launch types that the task definition was validated against.
awsEcsTaskDefinitionDetails_requiresCompatibilities :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionDetails_requiresCompatibilities = Lens.lens (\AwsEcsTaskDefinitionDetails' {requiresCompatibilities} -> requiresCompatibilities) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {requiresCompatibilities = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The placement constraint objects to use for tasks.
awsEcsTaskDefinitionDetails_placementConstraints :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionPlacementConstraintsDetails])
awsEcsTaskDefinitionDetails_placementConstraints = Lens.lens (\AwsEcsTaskDefinitionDetails' {placementConstraints} -> placementConstraints) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {placementConstraints = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of a family that this task definition is registered to.
awsEcsTaskDefinitionDetails_family :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_family = Lens.lens (\AwsEcsTaskDefinitionDetails' {family} -> family) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {family = a} :: AwsEcsTaskDefinitionDetails)

-- | The container definitions that describe the containers that make up the
-- task.
awsEcsTaskDefinitionDetails_containerDefinitions :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDetails])
awsEcsTaskDefinitionDetails_containerDefinitions = Lens.lens (\AwsEcsTaskDefinitionDetails' {containerDefinitions} -> containerDefinitions) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {containerDefinitions = a} :: AwsEcsTaskDefinitionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the task execution role that grants the container agent
-- permission to make API calls on behalf of the container user.
awsEcsTaskDefinitionDetails_executionRoleArn :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_executionRoleArn = Lens.lens (\AwsEcsTaskDefinitionDetails' {executionRoleArn} -> executionRoleArn) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {executionRoleArn = a} :: AwsEcsTaskDefinitionDetails)

-- | The Docker networking mode to use for the containers in the task. Valid
-- values are as follows:
--
-- -   @awsvpc@
--
-- -   @bridge@
--
-- -   @host@
--
-- -   @none@
awsEcsTaskDefinitionDetails_networkMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_networkMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {networkMode} -> networkMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {networkMode = a} :: AwsEcsTaskDefinitionDetails)

-- | The inter-process communication (IPC) resource namespace to use for the
-- containers in the task. Valid values are as follows:
--
-- -   @host@
--
-- -   @none@
--
-- -   @task@
awsEcsTaskDefinitionDetails_ipcMode :: Lens.Lens' AwsEcsTaskDefinitionDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionDetails_ipcMode = Lens.lens (\AwsEcsTaskDefinitionDetails' {ipcMode} -> ipcMode) (\s@AwsEcsTaskDefinitionDetails' {} a -> s {ipcMode = a} :: AwsEcsTaskDefinitionDetails)

instance Data.FromJSON AwsEcsTaskDefinitionDetails where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionDetails"
      ( \x ->
          AwsEcsTaskDefinitionDetails'
            Prelude.<$> (x Data..:? "ProxyConfiguration")
            Prelude.<*> (x Data..:? "PidMode")
            Prelude.<*> (x Data..:? "Cpu")
            Prelude.<*> (x Data..:? "Memory")
            Prelude.<*> (x Data..:? "TaskRoleArn")
            Prelude.<*> ( x Data..:? "InferenceAccelerators"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Volumes" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "RequiresCompatibilities"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "PlacementConstraints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Family")
            Prelude.<*> ( x Data..:? "ContainerDefinitions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ExecutionRoleArn")
            Prelude.<*> (x Data..:? "NetworkMode")
            Prelude.<*> (x Data..:? "IpcMode")
      )

instance Prelude.Hashable AwsEcsTaskDefinitionDetails where
  hashWithSalt _salt AwsEcsTaskDefinitionDetails' {..} =
    _salt `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` pidMode
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` taskRoleArn
      `Prelude.hashWithSalt` inferenceAccelerators
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` requiresCompatibilities
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` containerDefinitions
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` networkMode
      `Prelude.hashWithSalt` ipcMode

instance Prelude.NFData AwsEcsTaskDefinitionDetails where
  rnf AwsEcsTaskDefinitionDetails' {..} =
    Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf pidMode
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf taskRoleArn
      `Prelude.seq` Prelude.rnf inferenceAccelerators
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf requiresCompatibilities
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf containerDefinitions
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf networkMode
      `Prelude.seq` Prelude.rnf ipcMode

instance Data.ToJSON AwsEcsTaskDefinitionDetails where
  toJSON AwsEcsTaskDefinitionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProxyConfiguration" Data..=)
              Prelude.<$> proxyConfiguration,
            ("PidMode" Data..=) Prelude.<$> pidMode,
            ("Cpu" Data..=) Prelude.<$> cpu,
            ("Memory" Data..=) Prelude.<$> memory,
            ("TaskRoleArn" Data..=) Prelude.<$> taskRoleArn,
            ("InferenceAccelerators" Data..=)
              Prelude.<$> inferenceAccelerators,
            ("Volumes" Data..=) Prelude.<$> volumes,
            ("RequiresCompatibilities" Data..=)
              Prelude.<$> requiresCompatibilities,
            ("PlacementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("Family" Data..=) Prelude.<$> family,
            ("ContainerDefinitions" Data..=)
              Prelude.<$> containerDefinitions,
            ("ExecutionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("NetworkMode" Data..=) Prelude.<$> networkMode,
            ("IpcMode" Data..=) Prelude.<$> ipcMode
          ]
      )
