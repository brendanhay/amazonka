{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskOverride
  ( TaskOverride (..)
  -- * Smart constructor
  , mkTaskOverride
  -- * Lenses
  , toContainerOverrides
  , toCpu
  , toExecutionRoleArn
  , toInferenceAcceleratorOverrides
  , toMemory
  , toTaskRoleArn
  ) where

import qualified Network.AWS.ECS.Types.ContainerOverride as Types
import qualified Network.AWS.ECS.Types.InferenceAcceleratorOverride as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The overrides associated with a task.
--
-- /See:/ 'mkTaskOverride' smart constructor.
data TaskOverride = TaskOverride'
  { containerOverrides :: Core.Maybe [Types.ContainerOverride]
    -- ^ One or more container overrides sent to a task.
  , cpu :: Core.Maybe Core.Text
    -- ^ The cpu override for the task.
  , executionRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the task execution IAM role override for the task.
  , inferenceAcceleratorOverrides :: Core.Maybe [Types.InferenceAcceleratorOverride]
    -- ^ The Elastic Inference accelerator override for the task.
  , memory :: Core.Maybe Core.Text
    -- ^ The memory override for the task.
  , taskRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskOverride' value with any optional fields omitted.
mkTaskOverride
    :: TaskOverride
mkTaskOverride
  = TaskOverride'{containerOverrides = Core.Nothing,
                  cpu = Core.Nothing, executionRoleArn = Core.Nothing,
                  inferenceAcceleratorOverrides = Core.Nothing,
                  memory = Core.Nothing, taskRoleArn = Core.Nothing}

-- | One or more container overrides sent to a task.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toContainerOverrides :: Lens.Lens' TaskOverride (Core.Maybe [Types.ContainerOverride])
toContainerOverrides = Lens.field @"containerOverrides"
{-# INLINEABLE toContainerOverrides #-}
{-# DEPRECATED containerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead"  #-}

-- | The cpu override for the task.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toCpu :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
toCpu = Lens.field @"cpu"
{-# INLINEABLE toCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The Amazon Resource Name (ARN) of the task execution IAM role override for the task.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toExecutionRoleArn :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
toExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE toExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | The Elastic Inference accelerator override for the task.
--
-- /Note:/ Consider using 'inferenceAcceleratorOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toInferenceAcceleratorOverrides :: Lens.Lens' TaskOverride (Core.Maybe [Types.InferenceAcceleratorOverride])
toInferenceAcceleratorOverrides = Lens.field @"inferenceAcceleratorOverrides"
{-# INLINEABLE toInferenceAcceleratorOverrides #-}
{-# DEPRECATED inferenceAcceleratorOverrides "Use generic-lens or generic-optics with 'inferenceAcceleratorOverrides' instead"  #-}

-- | The memory override for the task.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toMemory :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
toMemory = Lens.field @"memory"
{-# INLINEABLE toMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
--
-- /Note:/ Consider using 'taskRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTaskRoleArn :: Lens.Lens' TaskOverride (Core.Maybe Core.Text)
toTaskRoleArn = Lens.field @"taskRoleArn"
{-# INLINEABLE toTaskRoleArn #-}
{-# DEPRECATED taskRoleArn "Use generic-lens or generic-optics with 'taskRoleArn' instead"  #-}

instance Core.FromJSON TaskOverride where
        toJSON TaskOverride{..}
          = Core.object
              (Core.catMaybes
                 [("containerOverrides" Core..=) Core.<$> containerOverrides,
                  ("cpu" Core..=) Core.<$> cpu,
                  ("executionRoleArn" Core..=) Core.<$> executionRoleArn,
                  ("inferenceAcceleratorOverrides" Core..=) Core.<$>
                    inferenceAcceleratorOverrides,
                  ("memory" Core..=) Core.<$> memory,
                  ("taskRoleArn" Core..=) Core.<$> taskRoleArn])

instance Core.FromJSON TaskOverride where
        parseJSON
          = Core.withObject "TaskOverride" Core.$
              \ x ->
                TaskOverride' Core.<$>
                  (x Core..:? "containerOverrides") Core.<*> x Core..:? "cpu"
                    Core.<*> x Core..:? "executionRoleArn"
                    Core.<*> x Core..:? "inferenceAcceleratorOverrides"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "taskRoleArn"
