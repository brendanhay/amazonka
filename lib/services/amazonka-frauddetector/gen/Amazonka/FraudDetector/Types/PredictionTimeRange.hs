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
-- Module      : Amazonka.FraudDetector.Types.PredictionTimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.PredictionTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time period for when the predictions were generated.
--
-- /See:/ 'newPredictionTimeRange' smart constructor.
data PredictionTimeRange = PredictionTimeRange'
  { -- | The start time of the time period for when the predictions were
    -- generated.
    startTime :: Prelude.Text,
    -- | The end time of the time period for when the predictions were generated.
    endTime :: Prelude.Text
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
-- 'startTime', 'predictionTimeRange_startTime' - The start time of the time period for when the predictions were
-- generated.
--
-- 'endTime', 'predictionTimeRange_endTime' - The end time of the time period for when the predictions were generated.
newPredictionTimeRange ::
  -- | 'startTime'
  Prelude.Text ->
  -- | 'endTime'
  Prelude.Text ->
  PredictionTimeRange
newPredictionTimeRange pStartTime_ pEndTime_ =
  PredictionTimeRange'
    { startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | The start time of the time period for when the predictions were
-- generated.
predictionTimeRange_startTime :: Lens.Lens' PredictionTimeRange Prelude.Text
predictionTimeRange_startTime = Lens.lens (\PredictionTimeRange' {startTime} -> startTime) (\s@PredictionTimeRange' {} a -> s {startTime = a} :: PredictionTimeRange)

-- | The end time of the time period for when the predictions were generated.
predictionTimeRange_endTime :: Lens.Lens' PredictionTimeRange Prelude.Text
predictionTimeRange_endTime = Lens.lens (\PredictionTimeRange' {endTime} -> endTime) (\s@PredictionTimeRange' {} a -> s {endTime = a} :: PredictionTimeRange)

instance Prelude.Hashable PredictionTimeRange where
  hashWithSalt _salt PredictionTimeRange' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData PredictionTimeRange where
  rnf PredictionTimeRange' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToJSON PredictionTimeRange where
  toJSON PredictionTimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("endTime" Core..= endTime)
          ]
      )
