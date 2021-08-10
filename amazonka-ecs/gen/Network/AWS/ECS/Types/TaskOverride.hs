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
-- Module      : Network.AWS.ECS.Types.TaskOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskOverride where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The overrides associated with a task.
--
-- /See:/ 'newTaskOverride' smart constructor.
data TaskOverride = TaskOverride'
  { -- | The Amazon Resource Name (ARN) of the IAM role that containers in this
    -- task can assume. All containers in this task are granted the permissions
    -- that are specified in this role.
    taskRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The memory override for the task.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task execution IAM role override
    -- for the task.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator override for the task.
    inferenceAcceleratorOverrides :: Prelude.Maybe [InferenceAcceleratorOverride],
    -- | One or more container overrides sent to a task.
    containerOverrides :: Prelude.Maybe [ContainerOverride],
    -- | The cpu override for the task.
    cpu :: Prelude.Maybe Prelude.Text
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
-- 'taskRoleArn', 'taskOverride_taskRoleArn' - The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role.
--
-- 'memory', 'taskOverride_memory' - The memory override for the task.
--
-- 'executionRoleArn', 'taskOverride_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task.
--
-- 'inferenceAcceleratorOverrides', 'taskOverride_inferenceAcceleratorOverrides' - The Elastic Inference accelerator override for the task.
--
-- 'containerOverrides', 'taskOverride_containerOverrides' - One or more container overrides sent to a task.
--
-- 'cpu', 'taskOverride_cpu' - The cpu override for the task.
newTaskOverride ::
  TaskOverride
newTaskOverride =
  TaskOverride'
    { taskRoleArn = Prelude.Nothing,
      memory = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      inferenceAcceleratorOverrides = Prelude.Nothing,
      containerOverrides = Prelude.Nothing,
      cpu = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role.
taskOverride_taskRoleArn :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_taskRoleArn = Lens.lens (\TaskOverride' {taskRoleArn} -> taskRoleArn) (\s@TaskOverride' {} a -> s {taskRoleArn = a} :: TaskOverride)

-- | The memory override for the task.
taskOverride_memory :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_memory = Lens.lens (\TaskOverride' {memory} -> memory) (\s@TaskOverride' {} a -> s {memory = a} :: TaskOverride)

-- | The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task.
taskOverride_executionRoleArn :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_executionRoleArn = Lens.lens (\TaskOverride' {executionRoleArn} -> executionRoleArn) (\s@TaskOverride' {} a -> s {executionRoleArn = a} :: TaskOverride)

-- | The Elastic Inference accelerator override for the task.
taskOverride_inferenceAcceleratorOverrides :: Lens.Lens' TaskOverride (Prelude.Maybe [InferenceAcceleratorOverride])
taskOverride_inferenceAcceleratorOverrides = Lens.lens (\TaskOverride' {inferenceAcceleratorOverrides} -> inferenceAcceleratorOverrides) (\s@TaskOverride' {} a -> s {inferenceAcceleratorOverrides = a} :: TaskOverride) Prelude.. Lens.mapping Lens._Coerce

-- | One or more container overrides sent to a task.
taskOverride_containerOverrides :: Lens.Lens' TaskOverride (Prelude.Maybe [ContainerOverride])
taskOverride_containerOverrides = Lens.lens (\TaskOverride' {containerOverrides} -> containerOverrides) (\s@TaskOverride' {} a -> s {containerOverrides = a} :: TaskOverride) Prelude.. Lens.mapping Lens._Coerce

-- | The cpu override for the task.
taskOverride_cpu :: Lens.Lens' TaskOverride (Prelude.Maybe Prelude.Text)
taskOverride_cpu = Lens.lens (\TaskOverride' {cpu} -> cpu) (\s@TaskOverride' {} a -> s {cpu = a} :: TaskOverride)

instance Core.FromJSON TaskOverride where
  parseJSON =
    Core.withObject
      "TaskOverride"
      ( \x ->
          TaskOverride'
            Prelude.<$> (x Core..:? "taskRoleArn")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "executionRoleArn")
            Prelude.<*> ( x Core..:? "inferenceAcceleratorOverrides"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "containerOverrides"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "cpu")
      )

instance Prelude.Hashable TaskOverride

instance Prelude.NFData TaskOverride

instance Core.ToJSON TaskOverride where
  toJSON TaskOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("taskRoleArn" Core..=) Prelude.<$> taskRoleArn,
            ("memory" Core..=) Prelude.<$> memory,
            ("executionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            ("inferenceAcceleratorOverrides" Core..=)
              Prelude.<$> inferenceAcceleratorOverrides,
            ("containerOverrides" Core..=)
              Prelude.<$> containerOverrides,
            ("cpu" Core..=) Prelude.<$> cpu
          ]
      )
