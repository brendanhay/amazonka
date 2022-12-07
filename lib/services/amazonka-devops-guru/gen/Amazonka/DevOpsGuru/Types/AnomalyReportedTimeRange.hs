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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time range that specifies when DevOps Guru opens and then closes an
-- anomaly. This is different from @AnomalyTimeRange@, which specifies the
-- time range when DevOps Guru actually observes the anomalous behavior.
--
-- /See:/ 'newAnomalyReportedTimeRange' smart constructor.
data AnomalyReportedTimeRange = AnomalyReportedTimeRange'
  { -- | The time when an anomaly is closed.
    closeTime :: Prelude.Maybe Data.POSIX,
    -- | The time when an anomaly is opened.
    openTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyReportedTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'closeTime', 'anomalyReportedTimeRange_closeTime' - The time when an anomaly is closed.
--
-- 'openTime', 'anomalyReportedTimeRange_openTime' - The time when an anomaly is opened.
newAnomalyReportedTimeRange ::
  -- | 'openTime'
  Prelude.UTCTime ->
  AnomalyReportedTimeRange
newAnomalyReportedTimeRange pOpenTime_ =
  AnomalyReportedTimeRange'
    { closeTime =
        Prelude.Nothing,
      openTime = Data._Time Lens.# pOpenTime_
    }

-- | The time when an anomaly is closed.
anomalyReportedTimeRange_closeTime :: Lens.Lens' AnomalyReportedTimeRange (Prelude.Maybe Prelude.UTCTime)
anomalyReportedTimeRange_closeTime = Lens.lens (\AnomalyReportedTimeRange' {closeTime} -> closeTime) (\s@AnomalyReportedTimeRange' {} a -> s {closeTime = a} :: AnomalyReportedTimeRange) Prelude.. Lens.mapping Data._Time

-- | The time when an anomaly is opened.
anomalyReportedTimeRange_openTime :: Lens.Lens' AnomalyReportedTimeRange Prelude.UTCTime
anomalyReportedTimeRange_openTime = Lens.lens (\AnomalyReportedTimeRange' {openTime} -> openTime) (\s@AnomalyReportedTimeRange' {} a -> s {openTime = a} :: AnomalyReportedTimeRange) Prelude.. Data._Time

instance Data.FromJSON AnomalyReportedTimeRange where
  parseJSON =
    Data.withObject
      "AnomalyReportedTimeRange"
      ( \x ->
          AnomalyReportedTimeRange'
            Prelude.<$> (x Data..:? "CloseTime")
            Prelude.<*> (x Data..: "OpenTime")
      )

instance Prelude.Hashable AnomalyReportedTimeRange where
  hashWithSalt _salt AnomalyReportedTimeRange' {..} =
    _salt `Prelude.hashWithSalt` closeTime
      `Prelude.hashWithSalt` openTime

instance Prelude.NFData AnomalyReportedTimeRange where
  rnf AnomalyReportedTimeRange' {..} =
    Prelude.rnf closeTime
      `Prelude.seq` Prelude.rnf openTime
