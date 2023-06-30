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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetContentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DatasetContentStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information about dataset contents.
--
-- /See:/ 'newDatasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { -- | The time the dataset content status was updated to SUCCEEDED or FAILED.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The actual time the creation of the dataset contents was started.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time the creation of the dataset contents was scheduled to start.
    scheduleTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the dataset contents.
    status :: Prelude.Maybe DatasetContentStatus,
    -- | The version of the dataset contents.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'datasetContentSummary_completionTime' - The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- 'creationTime', 'datasetContentSummary_creationTime' - The actual time the creation of the dataset contents was started.
--
-- 'scheduleTime', 'datasetContentSummary_scheduleTime' - The time the creation of the dataset contents was scheduled to start.
--
-- 'status', 'datasetContentSummary_status' - The status of the dataset contents.
--
-- 'version', 'datasetContentSummary_version' - The version of the dataset contents.
newDatasetContentSummary ::
  DatasetContentSummary
newDatasetContentSummary =
  DatasetContentSummary'
    { completionTime =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      scheduleTime = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
datasetContentSummary_completionTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_completionTime = Lens.lens (\DatasetContentSummary' {completionTime} -> completionTime) (\s@DatasetContentSummary' {} a -> s {completionTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Data._Time

-- | The actual time the creation of the dataset contents was started.
datasetContentSummary_creationTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_creationTime = Lens.lens (\DatasetContentSummary' {creationTime} -> creationTime) (\s@DatasetContentSummary' {} a -> s {creationTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Data._Time

-- | The time the creation of the dataset contents was scheduled to start.
datasetContentSummary_scheduleTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_scheduleTime = Lens.lens (\DatasetContentSummary' {scheduleTime} -> scheduleTime) (\s@DatasetContentSummary' {} a -> s {scheduleTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the dataset contents.
datasetContentSummary_status :: Lens.Lens' DatasetContentSummary (Prelude.Maybe DatasetContentStatus)
datasetContentSummary_status = Lens.lens (\DatasetContentSummary' {status} -> status) (\s@DatasetContentSummary' {} a -> s {status = a} :: DatasetContentSummary)

-- | The version of the dataset contents.
datasetContentSummary_version :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.Text)
datasetContentSummary_version = Lens.lens (\DatasetContentSummary' {version} -> version) (\s@DatasetContentSummary' {} a -> s {version = a} :: DatasetContentSummary)

instance Data.FromJSON DatasetContentSummary where
  parseJSON =
    Data.withObject
      "DatasetContentSummary"
      ( \x ->
          DatasetContentSummary'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "scheduleTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable DatasetContentSummary where
  hashWithSalt _salt DatasetContentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` scheduleTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version

instance Prelude.NFData DatasetContentSummary where
  rnf DatasetContentSummary' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf scheduleTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
