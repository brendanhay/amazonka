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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalyTimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalyTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time range that specifies when the observed unusual behavior in an
-- anomaly started and ended. This is different from
-- @AnomalyReportedTimeRange@, which specifies the time range when DevOps
-- Guru opens and then closes an anomaly.
--
-- /See:/ 'newAnomalyTimeRange' smart constructor.
data AnomalyTimeRange = AnomalyTimeRange'
  { -- | The time when the anomalous behavior ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the anomalous behavior started.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'anomalyTimeRange_endTime' - The time when the anomalous behavior ended.
--
-- 'startTime', 'anomalyTimeRange_startTime' - The time when the anomalous behavior started.
newAnomalyTimeRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  AnomalyTimeRange
newAnomalyTimeRange pStartTime_ =
  AnomalyTimeRange'
    { endTime = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The time when the anomalous behavior ended.
anomalyTimeRange_endTime :: Lens.Lens' AnomalyTimeRange (Prelude.Maybe Prelude.UTCTime)
anomalyTimeRange_endTime = Lens.lens (\AnomalyTimeRange' {endTime} -> endTime) (\s@AnomalyTimeRange' {} a -> s {endTime = a} :: AnomalyTimeRange) Prelude.. Lens.mapping Data._Time

-- | The time when the anomalous behavior started.
anomalyTimeRange_startTime :: Lens.Lens' AnomalyTimeRange Prelude.UTCTime
anomalyTimeRange_startTime = Lens.lens (\AnomalyTimeRange' {startTime} -> startTime) (\s@AnomalyTimeRange' {} a -> s {startTime = a} :: AnomalyTimeRange) Prelude.. Data._Time

instance Data.FromJSON AnomalyTimeRange where
  parseJSON =
    Data.withObject
      "AnomalyTimeRange"
      ( \x ->
          AnomalyTimeRange'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..: "StartTime")
      )

instance Prelude.Hashable AnomalyTimeRange where
  hashWithSalt _salt AnomalyTimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData AnomalyTimeRange where
  rnf AnomalyTimeRange' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf startTime
