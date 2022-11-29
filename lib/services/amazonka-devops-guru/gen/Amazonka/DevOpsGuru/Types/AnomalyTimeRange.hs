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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalyTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A time range that specifies when the observed unusual behavior in an
-- anomaly started and ended. This is different from
-- @AnomalyReportedTimeRange@, which specifies the time range when DevOps
-- Guru opens and then closes an anomaly.
--
-- /See:/ 'newAnomalyTimeRange' smart constructor.
data AnomalyTimeRange = AnomalyTimeRange'
  { -- | The time when the anomalous behavior ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The time when the anomalous behavior started.
    startTime :: Core.POSIX
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
      startTime = Core._Time Lens.# pStartTime_
    }

-- | The time when the anomalous behavior ended.
anomalyTimeRange_endTime :: Lens.Lens' AnomalyTimeRange (Prelude.Maybe Prelude.UTCTime)
anomalyTimeRange_endTime = Lens.lens (\AnomalyTimeRange' {endTime} -> endTime) (\s@AnomalyTimeRange' {} a -> s {endTime = a} :: AnomalyTimeRange) Prelude.. Lens.mapping Core._Time

-- | The time when the anomalous behavior started.
anomalyTimeRange_startTime :: Lens.Lens' AnomalyTimeRange Prelude.UTCTime
anomalyTimeRange_startTime = Lens.lens (\AnomalyTimeRange' {startTime} -> startTime) (\s@AnomalyTimeRange' {} a -> s {startTime = a} :: AnomalyTimeRange) Prelude.. Core._Time

instance Core.FromJSON AnomalyTimeRange where
  parseJSON =
    Core.withObject
      "AnomalyTimeRange"
      ( \x ->
          AnomalyTimeRange'
            Prelude.<$> (x Core..:? "EndTime")
            Prelude.<*> (x Core..: "StartTime")
      )

instance Prelude.Hashable AnomalyTimeRange where
  hashWithSalt _salt AnomalyTimeRange' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData AnomalyTimeRange where
  rnf AnomalyTimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
