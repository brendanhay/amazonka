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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.DataCollectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AssessmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about an assessment.
--
-- /See:/ 'newDataCollectionDetails' smart constructor.
data DataCollectionDetails = DataCollectionDetails'
  { -- | The time the assessment completes.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The number of failed servers in the assessment.
    failed :: Prelude.Maybe Prelude.Int,
    -- | The number of servers with the assessment status @IN_PROGESS@.
    inProgress :: Prelude.Maybe Prelude.Int,
    -- | The total number of servers in the assessment.
    servers :: Prelude.Maybe Prelude.Int,
    -- | The start time of assessment.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | The status message of the assessment.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The number of successful servers in the assessment.
    success :: Prelude.Maybe Prelude.Int
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
-- 'completionTime', 'dataCollectionDetails_completionTime' - The time the assessment completes.
--
-- 'failed', 'dataCollectionDetails_failed' - The number of failed servers in the assessment.
--
-- 'inProgress', 'dataCollectionDetails_inProgress' - The number of servers with the assessment status @IN_PROGESS@.
--
-- 'servers', 'dataCollectionDetails_servers' - The total number of servers in the assessment.
--
-- 'startTime', 'dataCollectionDetails_startTime' - The start time of assessment.
--
-- 'status', 'dataCollectionDetails_status' - The status of the assessment.
--
-- 'statusMessage', 'dataCollectionDetails_statusMessage' - The status message of the assessment.
--
-- 'success', 'dataCollectionDetails_success' - The number of successful servers in the assessment.
newDataCollectionDetails ::
  DataCollectionDetails
newDataCollectionDetails =
  DataCollectionDetails'
    { completionTime =
        Prelude.Nothing,
      failed = Prelude.Nothing,
      inProgress = Prelude.Nothing,
      servers = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      success = Prelude.Nothing
    }

-- | The time the assessment completes.
dataCollectionDetails_completionTime :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.UTCTime)
dataCollectionDetails_completionTime = Lens.lens (\DataCollectionDetails' {completionTime} -> completionTime) (\s@DataCollectionDetails' {} a -> s {completionTime = a} :: DataCollectionDetails) Prelude.. Lens.mapping Data._Time

-- | The number of failed servers in the assessment.
dataCollectionDetails_failed :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_failed = Lens.lens (\DataCollectionDetails' {failed} -> failed) (\s@DataCollectionDetails' {} a -> s {failed = a} :: DataCollectionDetails)

-- | The number of servers with the assessment status @IN_PROGESS@.
dataCollectionDetails_inProgress :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_inProgress = Lens.lens (\DataCollectionDetails' {inProgress} -> inProgress) (\s@DataCollectionDetails' {} a -> s {inProgress = a} :: DataCollectionDetails)

-- | The total number of servers in the assessment.
dataCollectionDetails_servers :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_servers = Lens.lens (\DataCollectionDetails' {servers} -> servers) (\s@DataCollectionDetails' {} a -> s {servers = a} :: DataCollectionDetails)

-- | The start time of assessment.
dataCollectionDetails_startTime :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.UTCTime)
dataCollectionDetails_startTime = Lens.lens (\DataCollectionDetails' {startTime} -> startTime) (\s@DataCollectionDetails' {} a -> s {startTime = a} :: DataCollectionDetails) Prelude.. Lens.mapping Data._Time

-- | The status of the assessment.
dataCollectionDetails_status :: Lens.Lens' DataCollectionDetails (Prelude.Maybe AssessmentStatus)
dataCollectionDetails_status = Lens.lens (\DataCollectionDetails' {status} -> status) (\s@DataCollectionDetails' {} a -> s {status = a} :: DataCollectionDetails)

-- | The status message of the assessment.
dataCollectionDetails_statusMessage :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Text)
dataCollectionDetails_statusMessage = Lens.lens (\DataCollectionDetails' {statusMessage} -> statusMessage) (\s@DataCollectionDetails' {} a -> s {statusMessage = a} :: DataCollectionDetails)

-- | The number of successful servers in the assessment.
dataCollectionDetails_success :: Lens.Lens' DataCollectionDetails (Prelude.Maybe Prelude.Int)
dataCollectionDetails_success = Lens.lens (\DataCollectionDetails' {success} -> success) (\s@DataCollectionDetails' {} a -> s {success = a} :: DataCollectionDetails)

instance Data.FromJSON DataCollectionDetails where
  parseJSON =
    Data.withObject
      "DataCollectionDetails"
      ( \x ->
          DataCollectionDetails'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "failed")
            Prelude.<*> (x Data..:? "inProgress")
            Prelude.<*> (x Data..:? "servers")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "success")
      )

instance Prelude.Hashable DataCollectionDetails where
  hashWithSalt _salt DataCollectionDetails' {..} =
    _salt `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` inProgress
      `Prelude.hashWithSalt` servers
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` success

instance Prelude.NFData DataCollectionDetails where
  rnf DataCollectionDetails' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf inProgress
      `Prelude.seq` Prelude.rnf servers
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf success
