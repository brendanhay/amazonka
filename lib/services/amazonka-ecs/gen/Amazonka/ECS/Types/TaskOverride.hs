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
-- Module      : Amazonka.ECS.Types.TaskOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ContainerOverride
import Amazonka.ECS.Types.EphemeralStorage
import Amazonka.ECS.Types.InferenceAcceleratorOverride
import qualified Amazonka.Prelude as Prelude

-- | The overrides that are associated with a task.
--
-- /See:/ 'newTaskOverride' smart constructor.
data TaskOverride = TaskOverride'
  { -- | One or more container overrides that are sent to a task.
    containerOverrides :: Prelude.Maybe [ContainerOverride],
    -- | The CPU override for the task.
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The ephemeral storage setting override for the task.
    --
    -- This parameter is only supported for tasks hosted on Fargate that use
    -- the following platform versions:
    --
    -- -   Linux platform version @1.4.0@ or later.
    --
    -- -   Windows platform version @1.0.0@ or later.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | The Amazon Resource Name (ARN) of the task execution IAM role override
    -- for the task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator override for the task.
    inferenceAcceleratorOverrides :: Prelude.Maybe [InferenceAcceleratorOverride],
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
-- Create a value of 'TaskOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerOverrides', 'taskOverride_containerOverrides' - One or more container overrides that are sent to a task.
--
-- 'cpu', 'taskOverride_cpu' - The CPU override for the task.
--
-- 'ephemeralStorage', 'taskOverride_ephemeralStorage' - The ephemeral storage setting override for the task.
--
-- This parameter is only supported for tasks hosted on Fargate that use
-- the following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
--
-- 'executionRoleArn', 'taskOverride_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'inferenceAcceleratorOverrides', 'taskOverride_inferenceAcceleratorOverrides' - The Elastic Inference accelerator override for the task.
--
-- 'memory', 'taskOverride_memory' - The memory override for the task.
--
-- 'taskRoleArn', 'taskOverride_taskRoleArn' - The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Role for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
newTaskOverride ::
  TaskOverride
newTaskOverride =
  TaskOverride'
    { containerOverrides = Prelude.Nothing,
      cpu = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      inferenceAcceleratorOverrides = Prelude.Nothing,
      memory = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing
    }

-- | One or more container overrides that are sent to a task.
taskOverride_containerOverrides :: Lens.Lens' TaskOverride (Prelude.Maybe [ContainerOverride])
taskOverride_containerOverrides = Lens.lens (\TaskOverride' {containerOverrides} -> containerOverrides) (\s@TaskOverride' {} a -> s {containerOverrides = a} :: TaskOverride) Prelude.. Lens.mapping Lens.coerced

-- | The CPU override for the task.
taskOverride_cpu :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_cpu = Lens.lens (\TaskOverride' {cpu} -> cpu) (\s@TaskOverride' {} a -> s {cpu = a} :: TaskOverride)

-- | The ephemeral storage setting override for the task.
--
-- This parameter is only supported for tasks hosted on Fargate that use
-- the following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
taskOverride_ephemeralStorage :: Lens.Lens' TaskOverride (Prelude.Maybe EphemeralStorage)
taskOverride_ephemeralStorage = Lens.lens (\TaskOverride' {ephemeralStorage} -> ephemeralStorage) (\s@TaskOverride' {} a -> s {ephemeralStorage = a} :: TaskOverride)

-- | The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskOverride_executionRoleArn :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_executionRoleArn = Lens.lens (\TaskOverride' {executionRoleArn} -> executionRoleArn) (\s@TaskOverride' {} a -> s {executionRoleArn = a} :: TaskOverride)

-- | The Elastic Inference accelerator override for the task.
taskOverride_inferenceAcceleratorOverrides :: Lens.Lens' TaskOverride (Prelude.Maybe [InferenceAcceleratorOverride])
taskOverride_inferenceAcceleratorOverrides = Lens.lens (\TaskOverride' {inferenceAcceleratorOverrides} -> inferenceAcceleratorOverrides) (\s@TaskOverride' {} a -> s {inferenceAcceleratorOverrides = a} :: TaskOverride) Prelude.. Lens.mapping Lens.coerced

-- | The memory override for the task.
taskOverride_memory :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_memory = Lens.lens (\TaskOverride' {memory} -> memory) (\s@TaskOverride' {} a -> s {memory = a} :: TaskOverride)

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Role for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskOverride_taskRoleArn :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_taskRoleArn = Lens.lens (\TaskOverride' {taskRoleArn} -> taskRoleArn) (\s@TaskOverride' {} a -> s {taskRoleArn = a} :: TaskOverride)

instance Data.FromJSON TaskOverride where
  parseJSON =
    Data.withObject
      "TaskOverride"
      ( \x ->
          TaskOverride'
            Prelude.<$> ( x Data..:? "containerOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "ephemeralStorage")
            Prelude.<*> (x Data..:? "executionRoleArn")
            Prelude.<*> ( x Data..:? "inferenceAcceleratorOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "taskRoleArn")
      )

instance Prelude.Hashable TaskOverride where
  hashWithSalt _salt TaskOverride' {..} =
    _salt `Prelude.hashWithSalt` containerOverrides
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inferenceAcceleratorOverrides
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` taskRoleArn

instance Prelude.NFData TaskOverride where
  rnf TaskOverride' {..} =
    Prelude.rnf containerOverrides
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inferenceAcceleratorOverrides
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf taskRoleArn

instance Data.ToJSON TaskOverride where
  toJSON TaskOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerOverrides" Data..=)
              Prelude.<$> containerOverrides,
            ("cpu" Data..=) Prelude.<$> cpu,
            ("ephemeralStorage" Data..=)
              Prelude.<$> ephemeralStorage,
            ("executionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("inferenceAcceleratorOverrides" Data..=)
              Prelude.<$> inferenceAcceleratorOverrides,
            ("memory" Data..=) Prelude.<$> memory,
            ("taskRoleArn" Data..=) Prelude.<$> taskRoleArn
          ]
      )
