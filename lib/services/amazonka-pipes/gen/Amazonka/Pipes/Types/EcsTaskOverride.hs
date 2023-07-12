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
-- Module      : Amazonka.Pipes.Types.EcsTaskOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsTaskOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.EcsContainerOverride
import Amazonka.Pipes.Types.EcsEphemeralStorage
import Amazonka.Pipes.Types.EcsInferenceAcceleratorOverride
import qualified Amazonka.Prelude as Prelude

-- | The overrides that are associated with a task.
--
-- /See:/ 'newEcsTaskOverride' smart constructor.
data EcsTaskOverride = EcsTaskOverride'
  { -- | One or more container overrides that are sent to a task.
    containerOverrides :: Prelude.Maybe [EcsContainerOverride],
    -- | The cpu override for the task.
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The ephemeral storage setting override for the task.
    --
    -- This parameter is only supported for tasks hosted on Fargate that use
    -- the following platform versions:
    --
    -- -   Linux platform version @1.4.0@ or later.
    --
    -- -   Windows platform version @1.0.0@ or later.
    ephemeralStorage :: Prelude.Maybe EcsEphemeralStorage,
    -- | The Amazon Resource Name (ARN) of the task execution IAM role override
    -- for the task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator override for the task.
    inferenceAcceleratorOverrides :: Prelude.Maybe [EcsInferenceAcceleratorOverride],
    -- | The memory override for the task.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that containers in this
    -- task can assume. All containers in this task are granted the permissions
    -- that are specified in this role. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Role for Tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    taskRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsTaskOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerOverrides', 'ecsTaskOverride_containerOverrides' - One or more container overrides that are sent to a task.
--
-- 'cpu', 'ecsTaskOverride_cpu' - The cpu override for the task.
--
-- 'ephemeralStorage', 'ecsTaskOverride_ephemeralStorage' - The ephemeral storage setting override for the task.
--
-- This parameter is only supported for tasks hosted on Fargate that use
-- the following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
--
-- 'executionRoleArn', 'ecsTaskOverride_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'inferenceAcceleratorOverrides', 'ecsTaskOverride_inferenceAcceleratorOverrides' - The Elastic Inference accelerator override for the task.
--
-- 'memory', 'ecsTaskOverride_memory' - The memory override for the task.
--
-- 'taskRoleArn', 'ecsTaskOverride_taskRoleArn' - The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Role for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
newEcsTaskOverride ::
  EcsTaskOverride
newEcsTaskOverride =
  EcsTaskOverride'
    { containerOverrides =
        Prelude.Nothing,
      cpu = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      inferenceAcceleratorOverrides = Prelude.Nothing,
      memory = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing
    }

-- | One or more container overrides that are sent to a task.
ecsTaskOverride_containerOverrides :: Lens.Lens' EcsTaskOverride (Prelude.Maybe [EcsContainerOverride])
ecsTaskOverride_containerOverrides = Lens.lens (\EcsTaskOverride' {containerOverrides} -> containerOverrides) (\s@EcsTaskOverride' {} a -> s {containerOverrides = a} :: EcsTaskOverride) Prelude.. Lens.mapping Lens.coerced

-- | The cpu override for the task.
ecsTaskOverride_cpu :: Lens.Lens' EcsTaskOverride (Prelude.Maybe Prelude.Text)
ecsTaskOverride_cpu = Lens.lens (\EcsTaskOverride' {cpu} -> cpu) (\s@EcsTaskOverride' {} a -> s {cpu = a} :: EcsTaskOverride)

-- | The ephemeral storage setting override for the task.
--
-- This parameter is only supported for tasks hosted on Fargate that use
-- the following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
ecsTaskOverride_ephemeralStorage :: Lens.Lens' EcsTaskOverride (Prelude.Maybe EcsEphemeralStorage)
ecsTaskOverride_ephemeralStorage = Lens.lens (\EcsTaskOverride' {ephemeralStorage} -> ephemeralStorage) (\s@EcsTaskOverride' {} a -> s {ephemeralStorage = a} :: EcsTaskOverride)

-- | The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
ecsTaskOverride_executionRoleArn :: Lens.Lens' EcsTaskOverride (Prelude.Maybe Prelude.Text)
ecsTaskOverride_executionRoleArn = Lens.lens (\EcsTaskOverride' {executionRoleArn} -> executionRoleArn) (\s@EcsTaskOverride' {} a -> s {executionRoleArn = a} :: EcsTaskOverride)

-- | The Elastic Inference accelerator override for the task.
ecsTaskOverride_inferenceAcceleratorOverrides :: Lens.Lens' EcsTaskOverride (Prelude.Maybe [EcsInferenceAcceleratorOverride])
ecsTaskOverride_inferenceAcceleratorOverrides = Lens.lens (\EcsTaskOverride' {inferenceAcceleratorOverrides} -> inferenceAcceleratorOverrides) (\s@EcsTaskOverride' {} a -> s {inferenceAcceleratorOverrides = a} :: EcsTaskOverride) Prelude.. Lens.mapping Lens.coerced

-- | The memory override for the task.
ecsTaskOverride_memory :: Lens.Lens' EcsTaskOverride (Prelude.Maybe Prelude.Text)
ecsTaskOverride_memory = Lens.lens (\EcsTaskOverride' {memory} -> memory) (\s@EcsTaskOverride' {} a -> s {memory = a} :: EcsTaskOverride)

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Role for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
ecsTaskOverride_taskRoleArn :: Lens.Lens' EcsTaskOverride (Prelude.Maybe Prelude.Text)
ecsTaskOverride_taskRoleArn = Lens.lens (\EcsTaskOverride' {taskRoleArn} -> taskRoleArn) (\s@EcsTaskOverride' {} a -> s {taskRoleArn = a} :: EcsTaskOverride)

instance Data.FromJSON EcsTaskOverride where
  parseJSON =
    Data.withObject
      "EcsTaskOverride"
      ( \x ->
          EcsTaskOverride'
            Prelude.<$> ( x
                            Data..:? "ContainerOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Cpu")
            Prelude.<*> (x Data..:? "EphemeralStorage")
            Prelude.<*> (x Data..:? "ExecutionRoleArn")
            Prelude.<*> ( x
                            Data..:? "InferenceAcceleratorOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Memory")
            Prelude.<*> (x Data..:? "TaskRoleArn")
      )

instance Prelude.Hashable EcsTaskOverride where
  hashWithSalt _salt EcsTaskOverride' {..} =
    _salt
      `Prelude.hashWithSalt` containerOverrides
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inferenceAcceleratorOverrides
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` taskRoleArn

instance Prelude.NFData EcsTaskOverride where
  rnf EcsTaskOverride' {..} =
    Prelude.rnf containerOverrides
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inferenceAcceleratorOverrides
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf taskRoleArn

instance Data.ToJSON EcsTaskOverride where
  toJSON EcsTaskOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerOverrides" Data..=)
              Prelude.<$> containerOverrides,
            ("Cpu" Data..=) Prelude.<$> cpu,
            ("EphemeralStorage" Data..=)
              Prelude.<$> ephemeralStorage,
            ("ExecutionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("InferenceAcceleratorOverrides" Data..=)
              Prelude.<$> inferenceAcceleratorOverrides,
            ("Memory" Data..=) Prelude.<$> memory,
            ("TaskRoleArn" Data..=) Prelude.<$> taskRoleArn
          ]
      )
