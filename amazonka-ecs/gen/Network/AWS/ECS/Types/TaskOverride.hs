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

-- | The overrides associated with a task.
--
-- /See:/ 'newTaskOverride' smart constructor.
data TaskOverride = TaskOverride'
  { -- | The Amazon Resource Name (ARN) of the IAM role that containers in this
    -- task can assume. All containers in this task are granted the permissions
    -- that are specified in this role.
    taskRoleArn :: Core.Maybe Core.Text,
    -- | The memory override for the task.
    memory :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the task execution IAM role override
    -- for the task.
    executionRoleArn :: Core.Maybe Core.Text,
    -- | The Elastic Inference accelerator override for the task.
    inferenceAcceleratorOverrides :: Core.Maybe [InferenceAcceleratorOverride],
    -- | One or more container overrides sent to a task.
    containerOverrides :: Core.Maybe [ContainerOverride],
    -- | The cpu override for the task.
    cpu :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { taskRoleArn = Core.Nothing,
      memory = Core.Nothing,
      executionRoleArn = Core.Nothing,
      inferenceAcceleratorOverrides = Core.Nothing,
      containerOverrides = Core.Nothing,
      cpu = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this
-- task can assume. All containers in this task are granted the permissions
-- that are specified in this role.
taskOverride_taskRoleArn :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
taskOverride_taskRoleArn = Lens.lens (\TaskOverride' {taskRoleArn} -> taskRoleArn) (\s@TaskOverride' {} a -> s {taskRoleArn = a} :: TaskOverride)

-- | The memory override for the task.
taskOverride_memory :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
taskOverride_memory = Lens.lens (\TaskOverride' {memory} -> memory) (\s@TaskOverride' {} a -> s {memory = a} :: TaskOverride)

-- | The Amazon Resource Name (ARN) of the task execution IAM role override
-- for the task.
taskOverride_executionRoleArn :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
taskOverride_executionRoleArn = Lens.lens (\TaskOverride' {executionRoleArn} -> executionRoleArn) (\s@TaskOverride' {} a -> s {executionRoleArn = a} :: TaskOverride)

-- | The Elastic Inference accelerator override for the task.
taskOverride_inferenceAcceleratorOverrides :: Lens.Lens' TaskOverride (Core.Maybe [InferenceAcceleratorOverride])
taskOverride_inferenceAcceleratorOverrides = Lens.lens (\TaskOverride' {inferenceAcceleratorOverrides} -> inferenceAcceleratorOverrides) (\s@TaskOverride' {} a -> s {inferenceAcceleratorOverrides = a} :: TaskOverride) Core.. Lens.mapping Lens._Coerce

-- | One or more container overrides sent to a task.
taskOverride_containerOverrides :: Lens.Lens' TaskOverride (Core.Maybe [ContainerOverride])
taskOverride_containerOverrides = Lens.lens (\TaskOverride' {containerOverrides} -> containerOverrides) (\s@TaskOverride' {} a -> s {containerOverrides = a} :: TaskOverride) Core.. Lens.mapping Lens._Coerce

-- | The cpu override for the task.
taskOverride_cpu :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
taskOverride_cpu = Lens.lens (\TaskOverride' {cpu} -> cpu) (\s@TaskOverride' {} a -> s {cpu = a} :: TaskOverride)

instance Core.FromJSON TaskOverride where
  parseJSON =
    Core.withObject
      "TaskOverride"
      ( \x ->
          TaskOverride'
            Core.<$> (x Core..:? "taskRoleArn")
            Core.<*> (x Core..:? "memory")
            Core.<*> (x Core..:? "executionRoleArn")
            Core.<*> ( x Core..:? "inferenceAcceleratorOverrides"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "containerOverrides"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "cpu")
      )

instance Core.Hashable TaskOverride

instance Core.NFData TaskOverride

instance Core.ToJSON TaskOverride where
  toJSON TaskOverride' {..} =
    Core.object
      ( Core.catMaybes
          [ ("taskRoleArn" Core..=) Core.<$> taskRoleArn,
            ("memory" Core..=) Core.<$> memory,
            ("executionRoleArn" Core..=)
              Core.<$> executionRoleArn,
            ("inferenceAcceleratorOverrides" Core..=)
              Core.<$> inferenceAcceleratorOverrides,
            ("containerOverrides" Core..=)
              Core.<$> containerOverrides,
            ("cpu" Core..=) Core.<$> cpu
          ]
      )
