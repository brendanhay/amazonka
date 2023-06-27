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
-- Module      : Amazonka.OsIs.Types.ChangeProgressStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.ChangeProgressStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types.ChangeProgressStage
import Amazonka.OsIs.Types.ChangeProgressStatuses
import qualified Amazonka.Prelude as Prelude

-- | The progress details of a pipeline configuration change.
--
-- /See:/ 'newChangeProgressStatus' smart constructor.
data ChangeProgressStatus = ChangeProgressStatus'
  { -- | Information about the stages that the pipeline is going through to
    -- perform the configuration change.
    changeProgressStages :: Prelude.Maybe [ChangeProgressStage],
    -- | The time at which the configuration change is made on the pipeline.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The overall status of the pipeline configuration change.
    status :: Prelude.Maybe ChangeProgressStatuses,
    -- | The total number of stages required for the pipeline configuration
    -- change.
    totalNumberOfStages :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeProgressStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeProgressStages', 'changeProgressStatus_changeProgressStages' - Information about the stages that the pipeline is going through to
-- perform the configuration change.
--
-- 'startTime', 'changeProgressStatus_startTime' - The time at which the configuration change is made on the pipeline.
--
-- 'status', 'changeProgressStatus_status' - The overall status of the pipeline configuration change.
--
-- 'totalNumberOfStages', 'changeProgressStatus_totalNumberOfStages' - The total number of stages required for the pipeline configuration
-- change.
newChangeProgressStatus ::
  ChangeProgressStatus
newChangeProgressStatus =
  ChangeProgressStatus'
    { changeProgressStages =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      totalNumberOfStages = Prelude.Nothing
    }

-- | Information about the stages that the pipeline is going through to
-- perform the configuration change.
changeProgressStatus_changeProgressStages :: Lens.Lens' ChangeProgressStatus (Prelude.Maybe [ChangeProgressStage])
changeProgressStatus_changeProgressStages = Lens.lens (\ChangeProgressStatus' {changeProgressStages} -> changeProgressStages) (\s@ChangeProgressStatus' {} a -> s {changeProgressStages = a} :: ChangeProgressStatus) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the configuration change is made on the pipeline.
changeProgressStatus_startTime :: Lens.Lens' ChangeProgressStatus (Prelude.Maybe Prelude.UTCTime)
changeProgressStatus_startTime = Lens.lens (\ChangeProgressStatus' {startTime} -> startTime) (\s@ChangeProgressStatus' {} a -> s {startTime = a} :: ChangeProgressStatus) Prelude.. Lens.mapping Data._Time

-- | The overall status of the pipeline configuration change.
changeProgressStatus_status :: Lens.Lens' ChangeProgressStatus (Prelude.Maybe ChangeProgressStatuses)
changeProgressStatus_status = Lens.lens (\ChangeProgressStatus' {status} -> status) (\s@ChangeProgressStatus' {} a -> s {status = a} :: ChangeProgressStatus)

-- | The total number of stages required for the pipeline configuration
-- change.
changeProgressStatus_totalNumberOfStages :: Lens.Lens' ChangeProgressStatus (Prelude.Maybe Prelude.Int)
changeProgressStatus_totalNumberOfStages = Lens.lens (\ChangeProgressStatus' {totalNumberOfStages} -> totalNumberOfStages) (\s@ChangeProgressStatus' {} a -> s {totalNumberOfStages = a} :: ChangeProgressStatus)

instance Data.FromJSON ChangeProgressStatus where
  parseJSON =
    Data.withObject
      "ChangeProgressStatus"
      ( \x ->
          ChangeProgressStatus'
            Prelude.<$> ( x
                            Data..:? "ChangeProgressStages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TotalNumberOfStages")
      )

instance Prelude.Hashable ChangeProgressStatus where
  hashWithSalt _salt ChangeProgressStatus' {..} =
    _salt
      `Prelude.hashWithSalt` changeProgressStages
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` totalNumberOfStages

instance Prelude.NFData ChangeProgressStatus where
  rnf ChangeProgressStatus' {..} =
    Prelude.rnf changeProgressStages
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf totalNumberOfStages
