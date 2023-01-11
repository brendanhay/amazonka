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
-- Module      : Amazonka.QuickSight.Types.TimeRangeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.ExcludePeriodConfiguration
import Amazonka.QuickSight.Types.FilterNullOption
import Amazonka.QuickSight.Types.TimeGranularity
import Amazonka.QuickSight.Types.TimeRangeFilterValue

-- | A @TimeRangeFilter@ filters values that are between two specified
-- values.
--
-- /See:/ 'newTimeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { -- | The exclude period of the time range filter.
    excludePeriodConfiguration :: Prelude.Maybe ExcludePeriodConfiguration,
    -- | Determines whether the maximum value in the filter value range should be
    -- included in the filtered results.
    includeMaximum :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the minimum value in the filter value range should be
    -- included in the filtered results.
    includeMinimum :: Prelude.Maybe Prelude.Bool,
    -- | The maximum value for the filter value range.
    rangeMaximumValue :: Prelude.Maybe TimeRangeFilterValue,
    -- | The minimum value for the filter value range.
    rangeMinimumValue :: Prelude.Maybe TimeRangeFilterValue,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | This option determines how null values should be treated when filtering
    -- data.
    --
    -- -   @ALL_VALUES@: Include null values in filtered results.
    --
    -- -   @NULLS_ONLY@: Only include null values in filtered results.
    --
    -- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
    nullOption :: FilterNullOption
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludePeriodConfiguration', 'timeRangeFilter_excludePeriodConfiguration' - The exclude period of the time range filter.
--
-- 'includeMaximum', 'timeRangeFilter_includeMaximum' - Determines whether the maximum value in the filter value range should be
-- included in the filtered results.
--
-- 'includeMinimum', 'timeRangeFilter_includeMinimum' - Determines whether the minimum value in the filter value range should be
-- included in the filtered results.
--
-- 'rangeMaximumValue', 'timeRangeFilter_rangeMaximumValue' - The maximum value for the filter value range.
--
-- 'rangeMinimumValue', 'timeRangeFilter_rangeMinimumValue' - The minimum value for the filter value range.
--
-- 'timeGranularity', 'timeRangeFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'filterId', 'timeRangeFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'timeRangeFilter_column' - The column that the filter is applied to.
--
-- 'nullOption', 'timeRangeFilter_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newTimeRangeFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  -- | 'nullOption'
  FilterNullOption ->
  TimeRangeFilter
newTimeRangeFilter pFilterId_ pColumn_ pNullOption_ =
  TimeRangeFilter'
    { excludePeriodConfiguration =
        Prelude.Nothing,
      includeMaximum = Prelude.Nothing,
      includeMinimum = Prelude.Nothing,
      rangeMaximumValue = Prelude.Nothing,
      rangeMinimumValue = Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      filterId = pFilterId_,
      column = pColumn_,
      nullOption = pNullOption_
    }

-- | The exclude period of the time range filter.
timeRangeFilter_excludePeriodConfiguration :: Lens.Lens' TimeRangeFilter (Prelude.Maybe ExcludePeriodConfiguration)
timeRangeFilter_excludePeriodConfiguration = Lens.lens (\TimeRangeFilter' {excludePeriodConfiguration} -> excludePeriodConfiguration) (\s@TimeRangeFilter' {} a -> s {excludePeriodConfiguration = a} :: TimeRangeFilter)

-- | Determines whether the maximum value in the filter value range should be
-- included in the filtered results.
timeRangeFilter_includeMaximum :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.Bool)
timeRangeFilter_includeMaximum = Lens.lens (\TimeRangeFilter' {includeMaximum} -> includeMaximum) (\s@TimeRangeFilter' {} a -> s {includeMaximum = a} :: TimeRangeFilter)

-- | Determines whether the minimum value in the filter value range should be
-- included in the filtered results.
timeRangeFilter_includeMinimum :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.Bool)
timeRangeFilter_includeMinimum = Lens.lens (\TimeRangeFilter' {includeMinimum} -> includeMinimum) (\s@TimeRangeFilter' {} a -> s {includeMinimum = a} :: TimeRangeFilter)

-- | The maximum value for the filter value range.
timeRangeFilter_rangeMaximumValue :: Lens.Lens' TimeRangeFilter (Prelude.Maybe TimeRangeFilterValue)
timeRangeFilter_rangeMaximumValue = Lens.lens (\TimeRangeFilter' {rangeMaximumValue} -> rangeMaximumValue) (\s@TimeRangeFilter' {} a -> s {rangeMaximumValue = a} :: TimeRangeFilter)

-- | The minimum value for the filter value range.
timeRangeFilter_rangeMinimumValue :: Lens.Lens' TimeRangeFilter (Prelude.Maybe TimeRangeFilterValue)
timeRangeFilter_rangeMinimumValue = Lens.lens (\TimeRangeFilter' {rangeMinimumValue} -> rangeMinimumValue) (\s@TimeRangeFilter' {} a -> s {rangeMinimumValue = a} :: TimeRangeFilter)

-- | The level of time precision that is used to aggregate @DateTime@ values.
timeRangeFilter_timeGranularity :: Lens.Lens' TimeRangeFilter (Prelude.Maybe TimeGranularity)
timeRangeFilter_timeGranularity = Lens.lens (\TimeRangeFilter' {timeGranularity} -> timeGranularity) (\s@TimeRangeFilter' {} a -> s {timeGranularity = a} :: TimeRangeFilter)

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
timeRangeFilter_filterId :: Lens.Lens' TimeRangeFilter Prelude.Text
timeRangeFilter_filterId = Lens.lens (\TimeRangeFilter' {filterId} -> filterId) (\s@TimeRangeFilter' {} a -> s {filterId = a} :: TimeRangeFilter)

-- | The column that the filter is applied to.
timeRangeFilter_column :: Lens.Lens' TimeRangeFilter ColumnIdentifier
timeRangeFilter_column = Lens.lens (\TimeRangeFilter' {column} -> column) (\s@TimeRangeFilter' {} a -> s {column = a} :: TimeRangeFilter)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
timeRangeFilter_nullOption :: Lens.Lens' TimeRangeFilter FilterNullOption
timeRangeFilter_nullOption = Lens.lens (\TimeRangeFilter' {nullOption} -> nullOption) (\s@TimeRangeFilter' {} a -> s {nullOption = a} :: TimeRangeFilter)

instance Data.FromJSON TimeRangeFilter where
  parseJSON =
    Data.withObject
      "TimeRangeFilter"
      ( \x ->
          TimeRangeFilter'
            Prelude.<$> (x Data..:? "ExcludePeriodConfiguration")
            Prelude.<*> (x Data..:? "IncludeMaximum")
            Prelude.<*> (x Data..:? "IncludeMinimum")
            Prelude.<*> (x Data..:? "RangeMaximumValue")
            Prelude.<*> (x Data..:? "RangeMinimumValue")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> (x Data..: "NullOption")
      )

instance Prelude.Hashable TimeRangeFilter where
  hashWithSalt _salt TimeRangeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` excludePeriodConfiguration
      `Prelude.hashWithSalt` includeMaximum
      `Prelude.hashWithSalt` includeMinimum
      `Prelude.hashWithSalt` rangeMaximumValue
      `Prelude.hashWithSalt` rangeMinimumValue
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData TimeRangeFilter where
  rnf TimeRangeFilter' {..} =
    Prelude.rnf excludePeriodConfiguration
      `Prelude.seq` Prelude.rnf includeMaximum
      `Prelude.seq` Prelude.rnf includeMinimum
      `Prelude.seq` Prelude.rnf rangeMaximumValue
      `Prelude.seq` Prelude.rnf rangeMinimumValue
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf nullOption

instance Data.ToJSON TimeRangeFilter where
  toJSON TimeRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludePeriodConfiguration" Data..=)
              Prelude.<$> excludePeriodConfiguration,
            ("IncludeMaximum" Data..=)
              Prelude.<$> includeMaximum,
            ("IncludeMinimum" Data..=)
              Prelude.<$> includeMinimum,
            ("RangeMaximumValue" Data..=)
              Prelude.<$> rangeMaximumValue,
            ("RangeMinimumValue" Data..=)
              Prelude.<$> rangeMinimumValue,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
