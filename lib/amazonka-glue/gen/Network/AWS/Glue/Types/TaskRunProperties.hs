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
    trpTaskType,
    trpExportLabelsTaskRunProperties,
    trpLabelingSetGenerationTaskRunProperties,
    trpFindMatchesTaskRunProperties,
    trpImportLabelsTaskRunProperties,
  )
where

import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration properties for the task run.
--
-- /See:/ 'mkTaskRunProperties' smart constructor.
data TaskRunProperties = TaskRunProperties'
  { -- | The type of task run.
    taskType :: Lude.Maybe TaskType,
    -- | The configuration properties for an exporting labels task run.
    exportLabelsTaskRunProperties :: Lude.Maybe ExportLabelsTaskRunProperties,
    -- | The configuration properties for a labeling set generation task run.
    labelingSetGenerationTaskRunProperties :: Lude.Maybe LabelingSetGenerationTaskRunProperties,
    -- | The configuration properties for a find matches task run.
    findMatchesTaskRunProperties :: Lude.Maybe FindMatchesTaskRunProperties,
    -- | The configuration properties for an importing labels task run.
    importLabelsTaskRunProperties :: Lude.Maybe ImportLabelsTaskRunProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskRunProperties' with the minimum fields required to make a request.
--
-- * 'taskType' - The type of task run.
-- * 'exportLabelsTaskRunProperties' - The configuration properties for an exporting labels task run.
-- * 'labelingSetGenerationTaskRunProperties' - The configuration properties for a labeling set generation task run.
-- * 'findMatchesTaskRunProperties' - The configuration properties for a find matches task run.
-- * 'importLabelsTaskRunProperties' - The configuration properties for an importing labels task run.
mkTaskRunProperties ::
  TaskRunProperties
mkTaskRunProperties =
  TaskRunProperties'
    { taskType = Lude.Nothing,
      exportLabelsTaskRunProperties = Lude.Nothing,
      labelingSetGenerationTaskRunProperties = Lude.Nothing,
      findMatchesTaskRunProperties = Lude.Nothing,
      importLabelsTaskRunProperties = Lude.Nothing
    }

-- | The type of task run.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpTaskType :: Lens.Lens' TaskRunProperties (Lude.Maybe TaskType)
trpTaskType = Lens.lens (taskType :: TaskRunProperties -> Lude.Maybe TaskType) (\s a -> s {taskType = a} :: TaskRunProperties)
{-# DEPRECATED trpTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The configuration properties for an exporting labels task run.
--
-- /Note:/ Consider using 'exportLabelsTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpExportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Lude.Maybe ExportLabelsTaskRunProperties)
trpExportLabelsTaskRunProperties = Lens.lens (exportLabelsTaskRunProperties :: TaskRunProperties -> Lude.Maybe ExportLabelsTaskRunProperties) (\s a -> s {exportLabelsTaskRunProperties = a} :: TaskRunProperties)
{-# DEPRECATED trpExportLabelsTaskRunProperties "Use generic-lens or generic-optics with 'exportLabelsTaskRunProperties' instead." #-}

-- | The configuration properties for a labeling set generation task run.
--
-- /Note:/ Consider using 'labelingSetGenerationTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpLabelingSetGenerationTaskRunProperties :: Lens.Lens' TaskRunProperties (Lude.Maybe LabelingSetGenerationTaskRunProperties)
trpLabelingSetGenerationTaskRunProperties = Lens.lens (labelingSetGenerationTaskRunProperties :: TaskRunProperties -> Lude.Maybe LabelingSetGenerationTaskRunProperties) (\s a -> s {labelingSetGenerationTaskRunProperties = a} :: TaskRunProperties)
{-# DEPRECATED trpLabelingSetGenerationTaskRunProperties "Use generic-lens or generic-optics with 'labelingSetGenerationTaskRunProperties' instead." #-}

-- | The configuration properties for a find matches task run.
--
-- /Note:/ Consider using 'findMatchesTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpFindMatchesTaskRunProperties :: Lens.Lens' TaskRunProperties (Lude.Maybe FindMatchesTaskRunProperties)
trpFindMatchesTaskRunProperties = Lens.lens (findMatchesTaskRunProperties :: TaskRunProperties -> Lude.Maybe FindMatchesTaskRunProperties) (\s a -> s {findMatchesTaskRunProperties = a} :: TaskRunProperties)
{-# DEPRECATED trpFindMatchesTaskRunProperties "Use generic-lens or generic-optics with 'findMatchesTaskRunProperties' instead." #-}

-- | The configuration properties for an importing labels task run.
--
-- /Note:/ Consider using 'importLabelsTaskRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpImportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Lude.Maybe ImportLabelsTaskRunProperties)
trpImportLabelsTaskRunProperties = Lens.lens (importLabelsTaskRunProperties :: TaskRunProperties -> Lude.Maybe ImportLabelsTaskRunProperties) (\s a -> s {importLabelsTaskRunProperties = a} :: TaskRunProperties)
{-# DEPRECATED trpImportLabelsTaskRunProperties "Use generic-lens or generic-optics with 'importLabelsTaskRunProperties' instead." #-}

instance Lude.FromJSON TaskRunProperties where
  parseJSON =
    Lude.withObject
      "TaskRunProperties"
      ( \x ->
          TaskRunProperties'
            Lude.<$> (x Lude..:? "TaskType")
            Lude.<*> (x Lude..:? "ExportLabelsTaskRunProperties")
            Lude.<*> (x Lude..:? "LabelingSetGenerationTaskRunProperties")
            Lude.<*> (x Lude..:? "FindMatchesTaskRunProperties")
            Lude.<*> (x Lude..:? "ImportLabelsTaskRunProperties")
      )
