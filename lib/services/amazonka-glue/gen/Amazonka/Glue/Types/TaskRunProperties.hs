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
-- Module      : Amazonka.Glue.Types.TaskRunProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TaskRunProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ExportLabelsTaskRunProperties
import Amazonka.Glue.Types.FindMatchesTaskRunProperties
import Amazonka.Glue.Types.ImportLabelsTaskRunProperties
import Amazonka.Glue.Types.LabelingSetGenerationTaskRunProperties
import Amazonka.Glue.Types.TaskType
import qualified Amazonka.Prelude as Prelude

-- | The configuration properties for the task run.
--
-- /See:/ 'newTaskRunProperties' smart constructor.
data TaskRunProperties = TaskRunProperties'
  { -- | The configuration properties for an importing labels task run.
    importLabelsTaskRunProperties :: Prelude.Maybe ImportLabelsTaskRunProperties,
    -- | The configuration properties for a find matches task run.
    findMatchesTaskRunProperties :: Prelude.Maybe FindMatchesTaskRunProperties,
    -- | The type of task run.
    taskType :: Prelude.Maybe TaskType,
    -- | The configuration properties for a labeling set generation task run.
    labelingSetGenerationTaskRunProperties :: Prelude.Maybe LabelingSetGenerationTaskRunProperties,
    -- | The configuration properties for an exporting labels task run.
    exportLabelsTaskRunProperties :: Prelude.Maybe ExportLabelsTaskRunProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importLabelsTaskRunProperties', 'taskRunProperties_importLabelsTaskRunProperties' - The configuration properties for an importing labels task run.
--
-- 'findMatchesTaskRunProperties', 'taskRunProperties_findMatchesTaskRunProperties' - The configuration properties for a find matches task run.
--
-- 'taskType', 'taskRunProperties_taskType' - The type of task run.
--
-- 'labelingSetGenerationTaskRunProperties', 'taskRunProperties_labelingSetGenerationTaskRunProperties' - The configuration properties for a labeling set generation task run.
--
-- 'exportLabelsTaskRunProperties', 'taskRunProperties_exportLabelsTaskRunProperties' - The configuration properties for an exporting labels task run.
newTaskRunProperties ::
  TaskRunProperties
newTaskRunProperties =
  TaskRunProperties'
    { importLabelsTaskRunProperties =
        Prelude.Nothing,
      findMatchesTaskRunProperties = Prelude.Nothing,
      taskType = Prelude.Nothing,
      labelingSetGenerationTaskRunProperties =
        Prelude.Nothing,
      exportLabelsTaskRunProperties = Prelude.Nothing
    }

-- | The configuration properties for an importing labels task run.
taskRunProperties_importLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe ImportLabelsTaskRunProperties)
taskRunProperties_importLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {importLabelsTaskRunProperties} -> importLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {importLabelsTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for a find matches task run.
taskRunProperties_findMatchesTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe FindMatchesTaskRunProperties)
taskRunProperties_findMatchesTaskRunProperties = Lens.lens (\TaskRunProperties' {findMatchesTaskRunProperties} -> findMatchesTaskRunProperties) (\s@TaskRunProperties' {} a -> s {findMatchesTaskRunProperties = a} :: TaskRunProperties)

-- | The type of task run.
taskRunProperties_taskType :: Lens.Lens' TaskRunProperties (Prelude.Maybe TaskType)
taskRunProperties_taskType = Lens.lens (\TaskRunProperties' {taskType} -> taskType) (\s@TaskRunProperties' {} a -> s {taskType = a} :: TaskRunProperties)

-- | The configuration properties for a labeling set generation task run.
taskRunProperties_labelingSetGenerationTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe LabelingSetGenerationTaskRunProperties)
taskRunProperties_labelingSetGenerationTaskRunProperties = Lens.lens (\TaskRunProperties' {labelingSetGenerationTaskRunProperties} -> labelingSetGenerationTaskRunProperties) (\s@TaskRunProperties' {} a -> s {labelingSetGenerationTaskRunProperties = a} :: TaskRunProperties)

-- | The configuration properties for an exporting labels task run.
taskRunProperties_exportLabelsTaskRunProperties :: Lens.Lens' TaskRunProperties (Prelude.Maybe ExportLabelsTaskRunProperties)
taskRunProperties_exportLabelsTaskRunProperties = Lens.lens (\TaskRunProperties' {exportLabelsTaskRunProperties} -> exportLabelsTaskRunProperties) (\s@TaskRunProperties' {} a -> s {exportLabelsTaskRunProperties = a} :: TaskRunProperties)

instance Data.FromJSON TaskRunProperties where
  parseJSON =
    Data.withObject
      "TaskRunProperties"
      ( \x ->
          TaskRunProperties'
            Prelude.<$> (x Data..:? "ImportLabelsTaskRunProperties")
            Prelude.<*> (x Data..:? "FindMatchesTaskRunProperties")
            Prelude.<*> (x Data..:? "TaskType")
            Prelude.<*> (x Data..:? "LabelingSetGenerationTaskRunProperties")
            Prelude.<*> (x Data..:? "ExportLabelsTaskRunProperties")
      )

instance Prelude.Hashable TaskRunProperties where
  hashWithSalt _salt TaskRunProperties' {..} =
    _salt
      `Prelude.hashWithSalt` importLabelsTaskRunProperties
      `Prelude.hashWithSalt` findMatchesTaskRunProperties
      `Prelude.hashWithSalt` taskType
      `Prelude.hashWithSalt` labelingSetGenerationTaskRunProperties
      `Prelude.hashWithSalt` exportLabelsTaskRunProperties

instance Prelude.NFData TaskRunProperties where
  rnf TaskRunProperties' {..} =
    Prelude.rnf importLabelsTaskRunProperties
      `Prelude.seq` Prelude.rnf findMatchesTaskRunProperties
      `Prelude.seq` Prelude.rnf taskType
      `Prelude.seq` Prelude.rnf labelingSetGenerationTaskRunProperties
      `Prelude.seq` Prelude.rnf exportLabelsTaskRunProperties
