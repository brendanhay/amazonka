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
-- Module      : Network.AWS.Glue.Types.TaskRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunProperties where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens

-- | The configuration properties for the task run.
--
-- /See:/ 'newTaskRunProperties' smart constructor.
data TaskRunProperties = TaskRunProperties'
  { -- | The configuration properties for an exporting labels task run.
    exportLabelsTaskRunProperties :: Core.Maybe ExportLabelsTaskRunProperties,
    -- | The configuration properties for a find matches task run.
    findMatchesTaskRunProperties :: Core.Maybe FindMatchesTaskRunProperties,
    -- | The configuration properties for a labeling set generation task run.
    labelingSetGenerationTaskRunProperties :: Core.Maybe LabelingSetGenerationTaskRunProperties,
    -- | The type of task run.
    taskType :: Core.Maybe TaskType,
    -- | The configuration properties for an importing labels task run.
    importLabelsTaskRunProperties :: Core.Maybe ImportLabelsTaskRunProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportLabelsTaskRunProperties', 'taskRunProperties_exportLabelsTaskRunProperties' - The configuration properties for an exporting labels task run.
--
-- 'findMatchesTaskRunProperties', 'taskRunProperties_findMatchesTaskRunProperties' - The configuration properties for a find matches task run.
--
-- 'labelingSetGenerationTaskRunProperties', 'taskRunProperties_labelingSetGenerationTaskRunProperties' - The configuration properties for a labeling set generation task run.
--
-- 'taskType', 'taskRunProperties_taskType' - The type of task run.
--
-- 'importLabelsTaskRunProperties', 'taskRunProperties_importLabelsTaskRunProperties' - The configuration properties for an importing labels task run.
newTaskRunProperties ::
  TaskRunProperties
newTaskRunProperties =
  TaskRunProperties'
    { exportLabelsTaskRunProperties =
        Core.Nothing,
      findMatchesTaskRunProperties = Core.Nothing,
      labelingSetGenerationTaskRunProperties =
        Core.Nothing,
      taskType = Core.Nothing,
      importLabelsTaskRunProperties = Core.Nothing
    }

-- | The configuration properties for an exporting labels task run.
taskRunProperties_exportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe ExportLabelsTaskRunProperties)
taskRunProperties_exportLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {exportLabelsTaskRunProperties} -> exportLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {exportLabelsTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for a find matches task run.
taskRunProperties_findMatchesTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe FindMatchesTaskRunProperties)
taskRunProperties_findMatchesTaskRunProperties = Lens.lens (\TaskRunProperties' {findMatchesTaskRunProperties} -> findMatchesTaskRunProperties) (\s@TaskRunProperties' {} a -> s {findMatchesTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for a labeling set generation task run.
taskRunProperties_labelingSetGenerationTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe LabelingSetGenerationTaskRunProperties)
taskRunProperties_labelingSetGenerationTaskRunProperties = Lens.lens (\TaskRunProperties' {labelingSetGenerationTaskRunProperties} -> labelingSetGenerationTaskRunProperties) (\s@TaskRunProperties' {} a -> s {labelingSetGenerationTaskRunProperties = a} :: TaskRunProperties)

-- | The type of task run.
taskRunProperties_taskType :: Lens.Lens' TaskRunProperties (Core.Maybe TaskType)
taskRunProperties_taskType = Lens.lens (\TaskRunProperties' {taskType} -> taskType) (\s@TaskRunProperties' {} a -> s {taskType = a} :: TaskRunProperties)

-- | The configuration properties for an importing labels task run.
taskRunProperties_importLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Core.Maybe ImportLabelsTaskRunProperties)
taskRunProperties_importLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {importLabelsTaskRunProperties} -> importLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {importLabelsTaskRunProperties = a} :: TaskRunProperties)

instance Core.FromJSON TaskRunProperties where
  parseJSON =
    Core.withObject
      "TaskRunProperties"
      ( \x ->
          TaskRunProperties'
            Core.<$> (x Core..:? "ExportLabelsTaskRunProperties")
            Core.<*> (x Core..:? "FindMatchesTaskRunProperties")
            Core.<*> (x Core..:? "LabelingSetGenerationTaskRunProperties")
            Core.<*> (x Core..:? "TaskType")
            Core.<*> (x Core..:? "ImportLabelsTaskRunProperties")
      )

instance Core.Hashable TaskRunProperties

instance Core.NFData TaskRunProperties
