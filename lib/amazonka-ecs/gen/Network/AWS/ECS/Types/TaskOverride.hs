{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskOverride
  ( TaskOverride (..),

    -- * Smart constructor
    mkTaskOverride,

    -- * Lenses
    toContainerOverrides,
    toExecutionRoleARN,
    toMemory,
    toTaskRoleARN,
    toInferenceAcceleratorOverrides,
    toCpu,
  )
where

import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The overrides associated with a task.
--
-- /See:/ 'mkTaskOverride' smart constructor.
data TaskOverride = TaskOverride'
  { containerOverrides ::
      Lude.Maybe [ContainerOverride],
    executionRoleARN :: Lude.Maybe Lude.Text,
    memory :: Lude.Maybe Lude.Text,
    taskRoleARN :: Lude.Maybe Lude.Text,
    inferenceAcceleratorOverrides ::
      Lude.Maybe [InferenceAcceleratorOverride],
    cpu :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskOverride' with the minimum fields required to make a request.
--
-- * 'containerOverrides' - One or more container overrides sent to a task.
-- * 'cpu' - The cpu override for the task.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the task execution IAM role override for the task.
-- * 'inferenceAcceleratorOverrides' - The Elastic Inference accelerator override for the task.
-- * 'memory' - The memory override for the task.
-- * 'taskRoleARN' - The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
mkTaskOverride ::
  TaskOverride
mkTaskOverride =
  TaskOverride'
    { containerOverrides = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      memory = Lude.Nothing,
      taskRoleARN = Lude.Nothing,
      inferenceAcceleratorOverrides = Lude.Nothing,
      cpu = Lude.Nothing
    }

-- | One or more container overrides sent to a task.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toContainerOverrides :: Lens.Lens' TaskOverride (Lude.Maybe [ContainerOverride])
toContainerOverrides = Lens.lens (containerOverrides :: TaskOverride -> Lude.Maybe [ContainerOverride]) (\s a -> s {containerOverrides = a} :: TaskOverride)
{-# DEPRECATED toContainerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead." #-}

-- | The Amazon Resource Name (ARN) of the task execution IAM role override for the task.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toExecutionRoleARN :: Lens.Lens' TaskOverride (Lude.Maybe Lude.Text)
toExecutionRoleARN = Lens.lens (executionRoleARN :: TaskOverride -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: TaskOverride)
{-# DEPRECATED toExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | The memory override for the task.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toMemory :: Lens.Lens' TaskOverride (Lude.Maybe Lude.Text)
toMemory = Lens.lens (memory :: TaskOverride -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: TaskOverride)
{-# DEPRECATED toMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
--
-- /Note:/ Consider using 'taskRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTaskRoleARN :: Lens.Lens' TaskOverride (Lude.Maybe Lude.Text)
toTaskRoleARN = Lens.lens (taskRoleARN :: TaskOverride -> Lude.Maybe Lude.Text) (\s a -> s {taskRoleARN = a} :: TaskOverride)
{-# DEPRECATED toTaskRoleARN "Use generic-lens or generic-optics with 'taskRoleARN' instead." #-}

-- | The Elastic Inference accelerator override for the task.
--
-- /Note:/ Consider using 'inferenceAcceleratorOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toInferenceAcceleratorOverrides :: Lens.Lens' TaskOverride (Lude.Maybe [InferenceAcceleratorOverride])
toInferenceAcceleratorOverrides = Lens.lens (inferenceAcceleratorOverrides :: TaskOverride -> Lude.Maybe [InferenceAcceleratorOverride]) (\s a -> s {inferenceAcceleratorOverrides = a} :: TaskOverride)
{-# DEPRECATED toInferenceAcceleratorOverrides "Use generic-lens or generic-optics with 'inferenceAcceleratorOverrides' instead." #-}

-- | The cpu override for the task.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toCpu :: Lens.Lens' TaskOverride (Lude.Maybe Lude.Text)
toCpu = Lens.lens (cpu :: TaskOverride -> Lude.Maybe Lude.Text) (\s a -> s {cpu = a} :: TaskOverride)
{-# DEPRECATED toCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

instance Lude.FromJSON TaskOverride where
  parseJSON =
    Lude.withObject
      "TaskOverride"
      ( \x ->
          TaskOverride'
            Lude.<$> (x Lude..:? "containerOverrides" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "executionRoleArn")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "taskRoleArn")
            Lude.<*> (x Lude..:? "inferenceAcceleratorOverrides" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "cpu")
      )

instance Lude.ToJSON TaskOverride where
  toJSON TaskOverride' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("containerOverrides" Lude..=) Lude.<$> containerOverrides,
            ("executionRoleArn" Lude..=) Lude.<$> executionRoleARN,
            ("memory" Lude..=) Lude.<$> memory,
            ("taskRoleArn" Lude..=) Lude.<$> taskRoleARN,
            ("inferenceAcceleratorOverrides" Lude..=)
              Lude.<$> inferenceAcceleratorOverrides,
            ("cpu" Lude..=) Lude.<$> cpu
          ]
      )
