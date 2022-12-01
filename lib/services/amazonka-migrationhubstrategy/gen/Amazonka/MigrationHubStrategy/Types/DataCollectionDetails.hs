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
-- Module      : Amazonka.MigrationHubStrategy.Types.DataCollectionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.DataCollectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.AssessmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about an assessment.
--
-- /See:/ 'newDataCollectionDetails' smart constructor.
data DataCollectionDetails = DataCollectionDetails'
  { -- | The number of failed servers in the assessment.
    failed :: Prelude.Maybe Prelude.Int,
    -- | The total number of servers in the assessment.
    servers :: Prelude.Maybe Prelude.Int,
    -- | The status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | The time the assessment completes.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The number of successful servers in the assessment.
    success :: Prelude.Maybe Prelude.Int,
    -- | The start time of assessment.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The number of servers with the assessment status @IN_PROGESS@.
    inProgress :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCollectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'dataCollectionDetails_failed' - The number of failed servers in the assessment.
--
-- 'servers', 'dataCollectionDetails_servers' - The total number of servers in the assessment.
--
-- 'status', 'dataCollectionDetails_status' - The status of the assessment.
--
-- 'completionTime', 'dataCollectionDetails_completionTime' - The time the assessment completes.
--
-- 'success', 'dataCollectionDetails_success' - The number of successful servers in the assessment.
--
-- 'startTime', 'dataCollectionDetails_startTime' - The start time of assessment.
--
-- 'inProgress', 'dataCollectionDetails_inProgress' - The number of servers with the assessment status @IN_PROGESS@.
newDataCollectionDetails ::
  DataCollectionDetails
newDataCollectionDetails =
  DataCollectionDetails'
    { failed = Prelude.Nothing,
      servers = Prelude.Nothing,
      status = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      success = Prelude.Nothing,
      startTime = Prelude.Nothing,
      inProgress = Prelude.Nothing
    }

-- | The number of failed servers in the assessment.
dataCollectionDetails_failed :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_failed = Lens.lens (\DataCollectionDetails' {failed} -> failed) (\s@DataCollectionDetails' {} a -> s {failed = a} :: DataCollectionDetails)

-- | The total number of servers in the assessment.
dataCollectionDetails_servers :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_servers = Lens.lens (\DataCollectionDetails' {servers} -> servers) (\s@DataCollectionDetails' {} a -> s {servers = a} :: DataCollectionDetails)

-- | The status of the assessment.
dataCollectionDetails_status :: Lens.Lens' DataCollectionDetails (Prelude.Maybe AssessmentStatus)
dataCollectionDetails_status = Lens.lens (\DataCollectionDetails' {status} -> status) (\s@DataCollectionDetails' {} a -> s {status = a} :: DataCollectionDetails)

-- | The time the assessment completes.
dataCollectionDetails_completionTime :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.UTCTime)
dataCollectionDetails_completionTime = Lens.lens (\DataCollectionDetails' {completionTime} -> completionTime) (\s@DataCollectionDetails' {} a -> s {completionTime = a} :: DataCollectionDetails) Prelude.. Lens.mapping Core._Time

-- | The number of successful servers in the assessment.
dataCollectionDetails_success :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_success = Lens.lens (\DataCollectionDetails' {success} -> success) (\s@DataCollectionDetails' {} a -> s {success = a} :: DataCollectionDetails)

-- | The start time of assessment.
dataCollectionDetails_startTime :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.UTCTime)
dataCollectionDetails_startTime = Lens.lens (\DataCollectionDetails' {startTime} -> startTime) (\s@DataCollectionDetails' {} a -> s {startTime = a} :: DataCollectionDetails) Prelude.. Lens.mapping Core._Time

-- | The number of servers with the assessment status @IN_PROGESS@.
dataCollectionDetails_inProgress :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_inProgress = Lens.lens (\DataCollectionDetails' {inProgress} -> inProgress) (\s@DataCollectionDetails' {} a -> s {inProgress = a} :: DataCollectionDetails)

instance Core.FromJSON DataCollectionDetails where
  parseJSON =
    Core.withObject
      "DataCollectionDetails"
      ( \x ->
          DataCollectionDetails'
            Prelude.<$> (x Core..:? "failed")
            Prelude.<*> (x Core..:? "servers")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "completionTime")
            Prelude.<*> (x Core..:? "success")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "inProgress")
      )

instance Prelude.Hashable DataCollectionDetails where
  hashWithSalt _salt DataCollectionDetails' {..} =
    _salt `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` servers
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` success
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` inProgress

instance Prelude.NFData DataCollectionDetails where
  rnf DataCollectionDetails' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf servers
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf success
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf inProgress
