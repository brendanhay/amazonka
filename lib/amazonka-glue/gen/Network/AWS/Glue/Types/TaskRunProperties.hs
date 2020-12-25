{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunProperties
  ( TaskRunProperties (..),

    -- * Smart constructor
    mkTaskRunProperties,

    -- * Lenses
    trpExportLabelsTaskRunProperties,
    trpFindMatchesTaskRunProperties,
    trpImportLabelsTaskRunProperties,
    trpLabelingSetGenerationTaskRunProperties,
    trpTaskType,
  )
where

import qualified Network.AWS.Glue.Types.ExportLabelsTaskRunProperties as Types
import qualified Network.AWS.Glue.Types.FindMatchesTaskRunProperties as Types
import qualified Network.AWS.Glue.Types.ImportLabelsTaskRunProperties as Types
import qualified Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties as Types
import qualified Network.AWS.Glue.Types.TaskType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration properties for the task run.
--
-- /See:/ 'mkTaskRunProperties' smart constructor.
data TaskRunProperties = TaskRunProperties'
  { -- | The configuration properties for an exporting labels task run.
    exportLabelsTaskRunProperties :: Core.Maybe Types.ExportLabelsTaskRunProperties,
    -- | The configuration properties for a find matches task run.
    findMatchesTaskRunProperties :: Core.Maybe Types.FindMatchesTaskRunProperties,
    -- | The configuration properties for an importing labels task run.
    importLabelsTaskRunProperties :: Core.Maybe Types.ImportLabelsTaskRunProperties,
    -- | The configuration properties for a labeling set generation task run.
    labelingSetGenerationTaskRunProperties :: Core.Maybe Types.LabelingSetGenerationTaskRunProperties,
    -- | The type of task run.
    taskType :: Core.Maybe Types.TaskType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskRunProperties' value with any optional fields omitted.
mkTaskRunProperties ::
  TaskRunProperties
mkTaskRunProperties =
  TaskRunProperties'
    { exportLabelsTaskRunProperties = Core.Nothing,
      findMatchesTaskRunProperties = Core.Nothing,
      importLabelsTaskRunProperties = Core.Nothing,
      labelingSetGenerationTaskRunProperties = Core.Nothing,
      taskType = Core.Nothing
    }

-- | The configuration properties for an exporting labels task run.
--
-- /Note:/ Consider using 'exportLabelsTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpExportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe Types.ExportLabelsTaskRunProperties)
trpExportLabelsTaskRunProperties = Lens.field @"exportLabelsTaskRunProperties"
{-# DEPRECATED trpExportLabelsTaskRunProperties "Use generic-lens or generic-optics with 'exportLabelsTaskRunProperties' instead." #-}

-- | The configuration properties for a find matches task run.
--
-- /Note:/ Consider using 'findMatchesTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpFindMatchesTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe Types.FindMatchesTaskRunProperties)
trpFindMatchesTaskRunProperties = Lens.field @"findMatchesTaskRunProperties"
{-# DEPRECATED trpFindMatchesTaskRunProperties "Use generic-lens or generic-optics with 'findMatchesTaskRunProperties' instead." #-}

-- | The configuration properties for an importing labels task run.
--
-- /Note:/ Consider using 'importLabelsTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpImportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe Types.ImportLabelsTaskRunProperties)
trpImportLabelsTaskRunProperties = Lens.field @"importLabelsTaskRunProperties"
{-# DEPRECATED trpImportLabelsTaskRunProperties "Use generic-lens or generic-optics with 'importLabelsTaskRunProperties' instead." #-}

-- | The configuration properties for a labeling set generation task run.
--
-- /Note:/ Consider using 'labelingSetGenerationTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpLabelingSetGenerationTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe Types.LabelingSetGenerationTaskRunProperties)
trpLabelingSetGenerationTaskRunProperties = Lens.field @"labelingSetGenerationTaskRunProperties"
{-# DEPRECATED trpLabelingSetGenerationTaskRunProperties "Use generic-lens or generic-optics with 'labelingSetGenerationTaskRunProperties' instead." #-}

-- | The type of task run.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpTaskType :: Lens.Lens' TaskRunProperties (Core.Maybe Types.TaskType)
trpTaskType = Lens.field @"taskType"
{-# DEPRECATED trpTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

instance Core.FromJSON TaskRunProperties where
  parseJSON =
    Core.withObject "TaskRunProperties" Core.$
      \x ->
        TaskRunProperties'
          Core.<$> (x Core..:? "ExportLabelsTaskRunProperties")
          Core.<*> (x Core..:? "FindMatchesTaskRunProperties")
          Core.<*> (x Core..:? "ImportLabelsTaskRunProperties")
          Core.<*> (x Core..:? "LabelingSetGenerationTaskRunProperties")
          Core.<*> (x Core..:? "TaskType")
