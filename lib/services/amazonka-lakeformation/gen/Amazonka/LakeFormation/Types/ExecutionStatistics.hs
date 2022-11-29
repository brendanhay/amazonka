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
-- Module      : Amazonka.LakeFormation.Types.ExecutionStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.ExecutionStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Statistics related to the processing of a query statement.
--
-- /See:/ 'newExecutionStatistics' smart constructor.
data ExecutionStatistics = ExecutionStatistics'
  { -- | The number of work units executed.
    workUnitsExecutedCount :: Prelude.Maybe Prelude.Integer,
    -- | The average time the request took to be executed.
    averageExecutionTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The amount of data that was scanned in bytes.
    dataScannedBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workUnitsExecutedCount', 'executionStatistics_workUnitsExecutedCount' - The number of work units executed.
--
-- 'averageExecutionTimeMillis', 'executionStatistics_averageExecutionTimeMillis' - The average time the request took to be executed.
--
-- 'dataScannedBytes', 'executionStatistics_dataScannedBytes' - The amount of data that was scanned in bytes.
newExecutionStatistics ::
  ExecutionStatistics
newExecutionStatistics =
  ExecutionStatistics'
    { workUnitsExecutedCount =
        Prelude.Nothing,
      averageExecutionTimeMillis = Prelude.Nothing,
      dataScannedBytes = Prelude.Nothing
    }

-- | The number of work units executed.
executionStatistics_workUnitsExecutedCount :: Lens.Lens' ExecutionStatistics (Prelude.Maybe Prelude.Integer)
executionStatistics_workUnitsExecutedCount = Lens.lens (\ExecutionStatistics' {workUnitsExecutedCount} -> workUnitsExecutedCount) (\s@ExecutionStatistics' {} a -> s {workUnitsExecutedCount = a} :: ExecutionStatistics)

-- | The average time the request took to be executed.
executionStatistics_averageExecutionTimeMillis :: Lens.Lens' ExecutionStatistics (Prelude.Maybe Prelude.Integer)
executionStatistics_averageExecutionTimeMillis = Lens.lens (\ExecutionStatistics' {averageExecutionTimeMillis} -> averageExecutionTimeMillis) (\s@ExecutionStatistics' {} a -> s {averageExecutionTimeMillis = a} :: ExecutionStatistics)

-- | The amount of data that was scanned in bytes.
executionStatistics_dataScannedBytes :: Lens.Lens' ExecutionStatistics (Prelude.Maybe Prelude.Integer)
executionStatistics_dataScannedBytes = Lens.lens (\ExecutionStatistics' {dataScannedBytes} -> dataScannedBytes) (\s@ExecutionStatistics' {} a -> s {dataScannedBytes = a} :: ExecutionStatistics)

instance Core.FromJSON ExecutionStatistics where
  parseJSON =
    Core.withObject
      "ExecutionStatistics"
      ( \x ->
          ExecutionStatistics'
            Prelude.<$> (x Core..:? "WorkUnitsExecutedCount")
            Prelude.<*> (x Core..:? "AverageExecutionTimeMillis")
            Prelude.<*> (x Core..:? "DataScannedBytes")
      )

instance Prelude.Hashable ExecutionStatistics where
  hashWithSalt _salt ExecutionStatistics' {..} =
    _salt `Prelude.hashWithSalt` workUnitsExecutedCount
      `Prelude.hashWithSalt` averageExecutionTimeMillis
      `Prelude.hashWithSalt` dataScannedBytes

instance Prelude.NFData ExecutionStatistics where
  rnf ExecutionStatistics' {..} =
    Prelude.rnf workUnitsExecutedCount
      `Prelude.seq` Prelude.rnf averageExecutionTimeMillis
      `Prelude.seq` Prelude.rnf dataScannedBytes
