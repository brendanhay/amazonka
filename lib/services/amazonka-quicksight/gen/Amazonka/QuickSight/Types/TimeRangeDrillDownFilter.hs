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
-- Module      : Amazonka.QuickSight.Types.TimeRangeDrillDownFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeRangeDrillDownFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.TimeGranularity

-- | The time range drill down filter.
--
-- /See:/ 'newTimeRangeDrillDownFilter' smart constructor.
data TimeRangeDrillDownFilter = TimeRangeDrillDownFilter'
  { -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The minimum value for the filter value range.
    rangeMinimum :: Data.POSIX,
    -- | The maximum value for the filter value range.
    rangeMaximum :: Data.POSIX,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: TimeGranularity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeDrillDownFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'timeRangeDrillDownFilter_column' - The column that the filter is applied to.
--
-- 'rangeMinimum', 'timeRangeDrillDownFilter_rangeMinimum' - The minimum value for the filter value range.
--
-- 'rangeMaximum', 'timeRangeDrillDownFilter_rangeMaximum' - The maximum value for the filter value range.
--
-- 'timeGranularity', 'timeRangeDrillDownFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
newTimeRangeDrillDownFilter ::
  -- | 'column'
  ColumnIdentifier ->
  -- | 'rangeMinimum'
  Prelude.UTCTime ->
  -- | 'rangeMaximum'
  Prelude.UTCTime ->
  -- | 'timeGranularity'
  TimeGranularity ->
  TimeRangeDrillDownFilter
newTimeRangeDrillDownFilter
  pColumn_
  pRangeMinimum_
  pRangeMaximum_
  pTimeGranularity_ =
    TimeRangeDrillDownFilter'
      { column = pColumn_,
        rangeMinimum = Data._Time Lens.# pRangeMinimum_,
        rangeMaximum = Data._Time Lens.# pRangeMaximum_,
        timeGranularity = pTimeGranularity_
      }

-- | The column that the filter is applied to.
timeRangeDrillDownFilter_column :: Lens.Lens' TimeRangeDrillDownFilter ColumnIdentifier
timeRangeDrillDownFilter_column = Lens.lens (\TimeRangeDrillDownFilter' {column} -> column) (\s@TimeRangeDrillDownFilter' {} a -> s {column = a} :: TimeRangeDrillDownFilter)

-- | The minimum value for the filter value range.
timeRangeDrillDownFilter_rangeMinimum :: Lens.Lens' TimeRangeDrillDownFilter Prelude.UTCTime
timeRangeDrillDownFilter_rangeMinimum = Lens.lens (\TimeRangeDrillDownFilter' {rangeMinimum} -> rangeMinimum) (\s@TimeRangeDrillDownFilter' {} a -> s {rangeMinimum = a} :: TimeRangeDrillDownFilter) Prelude.. Data._Time

-- | The maximum value for the filter value range.
timeRangeDrillDownFilter_rangeMaximum :: Lens.Lens' TimeRangeDrillDownFilter Prelude.UTCTime
timeRangeDrillDownFilter_rangeMaximum = Lens.lens (\TimeRangeDrillDownFilter' {rangeMaximum} -> rangeMaximum) (\s@TimeRangeDrillDownFilter' {} a -> s {rangeMaximum = a} :: TimeRangeDrillDownFilter) Prelude.. Data._Time

-- | The level of time precision that is used to aggregate @DateTime@ values.
timeRangeDrillDownFilter_timeGranularity :: Lens.Lens' TimeRangeDrillDownFilter TimeGranularity
timeRangeDrillDownFilter_timeGranularity = Lens.lens (\TimeRangeDrillDownFilter' {timeGranularity} -> timeGranularity) (\s@TimeRangeDrillDownFilter' {} a -> s {timeGranularity = a} :: TimeRangeDrillDownFilter)

instance Data.FromJSON TimeRangeDrillDownFilter where
  parseJSON =
    Data.withObject
      "TimeRangeDrillDownFilter"
      ( \x ->
          TimeRangeDrillDownFilter'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> (x Data..: "RangeMinimum")
            Prelude.<*> (x Data..: "RangeMaximum")
            Prelude.<*> (x Data..: "TimeGranularity")
      )

instance Prelude.Hashable TimeRangeDrillDownFilter where
  hashWithSalt _salt TimeRangeDrillDownFilter' {..} =
    _salt `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` rangeMinimum
      `Prelude.hashWithSalt` rangeMaximum
      `Prelude.hashWithSalt` timeGranularity

instance Prelude.NFData TimeRangeDrillDownFilter where
  rnf TimeRangeDrillDownFilter' {..} =
    Prelude.rnf column
      `Prelude.seq` Prelude.rnf rangeMinimum
      `Prelude.seq` Prelude.rnf rangeMaximum
      `Prelude.seq` Prelude.rnf timeGranularity

instance Data.ToJSON TimeRangeDrillDownFilter where
  toJSON TimeRangeDrillDownFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just ("RangeMinimum" Data..= rangeMinimum),
            Prelude.Just ("RangeMaximum" Data..= rangeMaximum),
            Prelude.Just
              ("TimeGranularity" Data..= timeGranularity)
          ]
      )
