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
-- Module      : Amazonka.LookoutMetrics.Types.TimeSeriesFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.TimeSeriesFeedback where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about feedback submitted for an anomalous metric.
--
-- /See:/ 'newTimeSeriesFeedback' smart constructor.
data TimeSeriesFeedback = TimeSeriesFeedback'
  { -- | Feedback on whether the metric is a legitimate anomaly.
    isAnomaly :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the metric.
    timeSeriesId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isAnomaly', 'timeSeriesFeedback_isAnomaly' - Feedback on whether the metric is a legitimate anomaly.
--
-- 'timeSeriesId', 'timeSeriesFeedback_timeSeriesId' - The ID of the metric.
newTimeSeriesFeedback ::
  TimeSeriesFeedback
newTimeSeriesFeedback =
  TimeSeriesFeedback'
    { isAnomaly = Prelude.Nothing,
      timeSeriesId = Prelude.Nothing
    }

-- | Feedback on whether the metric is a legitimate anomaly.
timeSeriesFeedback_isAnomaly :: Lens.Lens' TimeSeriesFeedback (Prelude.Maybe Prelude.Bool)
timeSeriesFeedback_isAnomaly = Lens.lens (\TimeSeriesFeedback' {isAnomaly} -> isAnomaly) (\s@TimeSeriesFeedback' {} a -> s {isAnomaly = a} :: TimeSeriesFeedback)

-- | The ID of the metric.
timeSeriesFeedback_timeSeriesId :: Lens.Lens' TimeSeriesFeedback (Prelude.Maybe Prelude.Text)
timeSeriesFeedback_timeSeriesId = Lens.lens (\TimeSeriesFeedback' {timeSeriesId} -> timeSeriesId) (\s@TimeSeriesFeedback' {} a -> s {timeSeriesId = a} :: TimeSeriesFeedback)

instance Core.FromJSON TimeSeriesFeedback where
  parseJSON =
    Core.withObject
      "TimeSeriesFeedback"
      ( \x ->
          TimeSeriesFeedback'
            Prelude.<$> (x Core..:? "IsAnomaly")
            Prelude.<*> (x Core..:? "TimeSeriesId")
      )

instance Prelude.Hashable TimeSeriesFeedback where
  hashWithSalt _salt TimeSeriesFeedback' {..} =
    _salt `Prelude.hashWithSalt` isAnomaly
      `Prelude.hashWithSalt` timeSeriesId

instance Prelude.NFData TimeSeriesFeedback where
  rnf TimeSeriesFeedback' {..} =
    Prelude.rnf isAnomaly
      `Prelude.seq` Prelude.rnf timeSeriesId
