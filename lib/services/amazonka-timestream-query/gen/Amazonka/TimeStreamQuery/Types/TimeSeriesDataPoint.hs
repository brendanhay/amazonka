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
-- Module      : Amazonka.TimeStreamQuery.Types.TimeSeriesDataPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.TimeSeriesDataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.Datum

-- | The timeseries data type represents the values of a measure over time. A
-- time series is an array of rows of timestamps and measure values, with
-- rows sorted in ascending order of time. A TimeSeriesDataPoint is a
-- single data point in the time series. It represents a tuple of (time,
-- measure value) in a time series.
--
-- /See:/ 'newTimeSeriesDataPoint' smart constructor.
data TimeSeriesDataPoint = TimeSeriesDataPoint'
  { -- | The timestamp when the measure value was collected.
    time :: Prelude.Text,
    -- | The measure value for the data point.
    value :: Datum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesDataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'time', 'timeSeriesDataPoint_time' - The timestamp when the measure value was collected.
--
-- 'value', 'timeSeriesDataPoint_value' - The measure value for the data point.
newTimeSeriesDataPoint ::
  -- | 'time'
  Prelude.Text ->
  -- | 'value'
  Datum ->
  TimeSeriesDataPoint
newTimeSeriesDataPoint pTime_ pValue_ =
  TimeSeriesDataPoint'
    { time = pTime_,
      value = pValue_
    }

-- | The timestamp when the measure value was collected.
timeSeriesDataPoint_time :: Lens.Lens' TimeSeriesDataPoint Prelude.Text
timeSeriesDataPoint_time = Lens.lens (\TimeSeriesDataPoint' {time} -> time) (\s@TimeSeriesDataPoint' {} a -> s {time = a} :: TimeSeriesDataPoint)

-- | The measure value for the data point.
timeSeriesDataPoint_value :: Lens.Lens' TimeSeriesDataPoint Datum
timeSeriesDataPoint_value = Lens.lens (\TimeSeriesDataPoint' {value} -> value) (\s@TimeSeriesDataPoint' {} a -> s {value = a} :: TimeSeriesDataPoint)

instance Data.FromJSON TimeSeriesDataPoint where
  parseJSON =
    Data.withObject
      "TimeSeriesDataPoint"
      ( \x ->
          TimeSeriesDataPoint'
            Prelude.<$> (x Data..: "Time")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable TimeSeriesDataPoint where
  hashWithSalt _salt TimeSeriesDataPoint' {..} =
    _salt
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` value

instance Prelude.NFData TimeSeriesDataPoint where
  rnf TimeSeriesDataPoint' {..} =
    Prelude.rnf time `Prelude.seq` Prelude.rnf value
