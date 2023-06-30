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
-- Module      : Amazonka.DevOpsGuru.Types.PredictionTimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PredictionTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The time range during which anomalous behavior in a proactive anomaly or
-- an insight is expected to occur.
--
-- /See:/ 'newPredictionTimeRange' smart constructor.
data PredictionTimeRange = PredictionTimeRange'
  { -- | The time when the behavior in a proactive insight is expected to end.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time range during which a metric limit is expected to be exceeded.
    -- This applies to proactive insights only.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictionTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'predictionTimeRange_endTime' - The time when the behavior in a proactive insight is expected to end.
--
-- 'startTime', 'predictionTimeRange_startTime' - The time range during which a metric limit is expected to be exceeded.
-- This applies to proactive insights only.
newPredictionTimeRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  PredictionTimeRange
newPredictionTimeRange pStartTime_ =
  PredictionTimeRange'
    { endTime = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The time when the behavior in a proactive insight is expected to end.
predictionTimeRange_endTime :: Lens.Lens' PredictionTimeRange (Prelude.Maybe Prelude.UTCTime)
predictionTimeRange_endTime = Lens.lens (\PredictionTimeRange' {endTime} -> endTime) (\s@PredictionTimeRange' {} a -> s {endTime = a} :: PredictionTimeRange) Prelude.. Lens.mapping Data._Time

-- | The time range during which a metric limit is expected to be exceeded.
-- This applies to proactive insights only.
predictionTimeRange_startTime :: Lens.Lens' PredictionTimeRange Prelude.UTCTime
predictionTimeRange_startTime = Lens.lens (\PredictionTimeRange' {startTime} -> startTime) (\s@PredictionTimeRange' {} a -> s {startTime = a} :: PredictionTimeRange) Prelude.. Data._Time

instance Data.FromJSON PredictionTimeRange where
  parseJSON =
    Data.withObject
      "PredictionTimeRange"
      ( \x ->
          PredictionTimeRange'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..: "StartTime")
      )

instance Prelude.Hashable PredictionTimeRange where
  hashWithSalt _salt PredictionTimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData PredictionTimeRange where
  rnf PredictionTimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
