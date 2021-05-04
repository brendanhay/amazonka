{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration properties for the task run.
--
-- /See:/ 'newTaskRunProperties' smart constructor.
data TaskRunProperties = TaskRunProperties'
  { -- | The configuration properties for an exporting labels task run.
    exportLabelsTaskRunProperties :: Prelude.Maybe ExportLabelsTaskRunProperties,
    -- | The configuration properties for a find matches task run.
    findMatchesTaskRunProperties :: Prelude.Maybe FindMatchesTaskRunProperties,
    -- | The configuration properties for a labeling set generation task run.
    labelingSetGenerationTaskRunProperties :: Prelude.Maybe LabelingSetGenerationTaskRunProperties,
    -- | The type of task run.
    taskType :: Prelude.Maybe TaskType,
    -- | The configuration properties for an importing labels task run.
    importLabelsTaskRunProperties :: Prelude.Maybe ImportLabelsTaskRunProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      findMatchesTaskRunProperties = Prelude.Nothing,
      labelingSetGenerationTaskRunProperties =
        Prelude.Nothing,
      taskType = Prelude.Nothing,
      importLabelsTaskRunProperties = Prelude.Nothing
    }

-- | The configuration properties for an exporting labels task run.
taskRunProperties_exportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe ExportLabelsTaskRunProperties)
taskRunProperties_exportLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {exportLabelsTaskRunProperties} -> exportLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {exportLabelsTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for a find matches task run.
taskRunProperties_findMatchesTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe FindMatchesTaskRunProperties)
taskRunProperties_findMatchesTaskRunProperties = Lens.lens (\TaskRunProperties' {findMatchesTaskRunProperties} -> findMatchesTaskRunProperties) (\s@TaskRunProperties' {} a -> s {findMatchesTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for a labeling set generation task run.
taskRunProperties_labelingSetGenerationTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe LabelingSetGenerationTaskRunProperties)
taskRunProperties_labelingSetGenerationTaskRunProperties = Lens.lens (\TaskRunProperties' {labelingSetGenerationTaskRunProperties} -> labelingSetGenerationTaskRunProperties) (\s@TaskRunProperties' {} a -> s {labelingSetGenerationTaskRunProperties = a} :: TaskRunProperties)

-- | The type of task run.
taskRunProperties_taskType :: Lens.Lens' TaskRunProperties (Prelude.Maybe TaskType)
taskRunProperties_taskType = Lens.lens (\TaskRunProperties' {taskType} -> taskType) (\s@TaskRunProperties' {} a -> s {taskType = a} :: TaskRunProperties)

-- | The configuration properties for an importing labels task run.
taskRunProperties_importLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe ImportLabelsTaskRunProperties)
taskRunProperties_importLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {importLabelsTaskRunProperties} -> importLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {importLabelsTaskRunProperties = a} :: TaskRunProperties)

instance Prelude.FromJSON TaskRunProperties where
  parseJSON =
    Prelude.withObject
      "TaskRunProperties"
      ( \x ->
          TaskRunProperties'
            Prelude.<$> (x Prelude..:? "ExportLabelsTaskRunProperties")
            Prelude.<*> (x Prelude..:? "FindMatchesTaskRunProperties")
            Prelude.<*> ( x
                            Prelude..:? "LabelingSetGenerationTaskRunProperties"
                        )
            Prelude.<*> (x Prelude..:? "TaskType")
            Prelude.<*> (x Prelude..:? "ImportLabelsTaskRunProperties")
      )

instance Prelude.Hashable TaskRunProperties

instance Prelude.NFData TaskRunProperties
