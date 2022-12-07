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
-- Module      : Amazonka.M2.Types.DataSetImportTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetImportTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.DataSetImportSummary
import Amazonka.M2.Types.DataSetTaskLifecycle
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a data set import task.
--
-- /See:/ 'newDataSetImportTask' smart constructor.
data DataSetImportTask = DataSetImportTask'
  { -- | The status of the data set import task.
    status :: DataSetTaskLifecycle,
    -- | A summary of the data set import task.
    summary :: DataSetImportSummary,
    -- | The identifier of the data set import task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataSetImportTask_status' - The status of the data set import task.
--
-- 'summary', 'dataSetImportTask_summary' - A summary of the data set import task.
--
-- 'taskId', 'dataSetImportTask_taskId' - The identifier of the data set import task.
newDataSetImportTask ::
  -- | 'status'
  DataSetTaskLifecycle ->
  -- | 'summary'
  DataSetImportSummary ->
  -- | 'taskId'
  Prelude.Text ->
  DataSetImportTask
newDataSetImportTask pStatus_ pSummary_ pTaskId_ =
  DataSetImportTask'
    { status = pStatus_,
      summary = pSummary_,
      taskId = pTaskId_
    }

-- | The status of the data set import task.
dataSetImportTask_status :: Lens.Lens' DataSetImportTask DataSetTaskLifecycle
dataSetImportTask_status = Lens.lens (\DataSetImportTask' {status} -> status) (\s@DataSetImportTask' {} a -> s {status = a} :: DataSetImportTask)

-- | A summary of the data set import task.
dataSetImportTask_summary :: Lens.Lens' DataSetImportTask DataSetImportSummary
dataSetImportTask_summary = Lens.lens (\DataSetImportTask' {summary} -> summary) (\s@DataSetImportTask' {} a -> s {summary = a} :: DataSetImportTask)

-- | The identifier of the data set import task.
dataSetImportTask_taskId :: Lens.Lens' DataSetImportTask Prelude.Text
dataSetImportTask_taskId = Lens.lens (\DataSetImportTask' {taskId} -> taskId) (\s@DataSetImportTask' {} a -> s {taskId = a} :: DataSetImportTask)

instance Data.FromJSON DataSetImportTask where
  parseJSON =
    Data.withObject
      "DataSetImportTask"
      ( \x ->
          DataSetImportTask'
            Prelude.<$> (x Data..: "status")
            Prelude.<*> (x Data..: "summary")
            Prelude.<*> (x Data..: "taskId")
      )

instance Prelude.Hashable DataSetImportTask where
  hashWithSalt _salt DataSetImportTask' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData DataSetImportTask where
  rnf DataSetImportTask' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf taskId
