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
-- Module      : Amazonka.QuickSight.Types.NumericRangeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.FilterNullOption
import Amazonka.QuickSight.Types.NumericFilterSelectAllOptions
import Amazonka.QuickSight.Types.NumericRangeFilterValue

-- | A @NumericRangeFilter@ filters values that are within the value range.
--
-- /See:/ 'newNumericRangeFilter' smart constructor.
data NumericRangeFilter = NumericRangeFilter'
  { -- | The aggregation function of the filter.
    aggregationFunction :: Prelude.Maybe AggregationFunction,
    -- | Determines whether the maximum value in the filter value range should be
    -- included in the filtered results.
    includeMaximum :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the minimum value in the filter value range should be
    -- included in the filtered results.
    includeMinimum :: Prelude.Maybe Prelude.Bool,
    -- | The maximum value for the filter value range.
    rangeMaximum :: Prelude.Maybe NumericRangeFilterValue,
    -- | The minimum value for the filter value range.
    rangeMinimum :: Prelude.Maybe NumericRangeFilterValue,
    -- | Select all of the values. Null is not the assigned value of select all.
    --
    -- -   @FILTER_ALL_VALUES@
    selectAllOptions :: Prelude.Maybe NumericFilterSelectAllOptions,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'numericRangeFilter_aggregationFunction' - The aggregation function of the filter.
--
-- 'includeMaximum', 'numericRangeFilter_includeMaximum' - Determines whether the maximum value in the filter value range should be
-- included in the filtered results.
--
-- 'includeMinimum', 'numericRangeFilter_includeMinimum' - Determines whether the minimum value in the filter value range should be
-- included in the filtered results.
--
-- 'rangeMaximum', 'numericRangeFilter_rangeMaximum' - The maximum value for the filter value range.
--
-- 'rangeMinimum', 'numericRangeFilter_rangeMinimum' - The minimum value for the filter value range.
--
-- 'selectAllOptions', 'numericRangeFilter_selectAllOptions' - Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
--
-- 'filterId', 'numericRangeFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'numericRangeFilter_column' - The column that the filter is applied to.
--
-- 'nullOption', 'numericRangeFilter_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newNumericRangeFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  -- | 'nullOption'
  FilterNullOption ->
  NumericRangeFilter
newNumericRangeFilter
  pFilterId_
  pColumn_
  pNullOption_ =
    NumericRangeFilter'
      { aggregationFunction =
          Prelude.Nothing,
        includeMaximum = Prelude.Nothing,
        includeMinimum = Prelude.Nothing,
        rangeMaximum = Prelude.Nothing,
        rangeMinimum = Prelude.Nothing,
        selectAllOptions = Prelude.Nothing,
        filterId = pFilterId_,
        column = pColumn_,
        nullOption = pNullOption_
      }

-- | The aggregation function of the filter.
numericRangeFilter_aggregationFunction :: Lens.Lens' NumericRangeFilter (Prelude.Maybe AggregationFunction)
numericRangeFilter_aggregationFunction = Lens.lens (\NumericRangeFilter' {aggregationFunction} -> aggregationFunction) (\s@NumericRangeFilter' {} a -> s {aggregationFunction = a} :: NumericRangeFilter)

-- | Determines whether the maximum value in the filter value range should be
-- included in the filtered results.
numericRangeFilter_includeMaximum :: Lens.Lens' NumericRangeFilter (Prelude.Maybe Prelude.Bool)
numericRangeFilter_includeMaximum = Lens.lens (\NumericRangeFilter' {includeMaximum} -> includeMaximum) (\s@NumericRangeFilter' {} a -> s {includeMaximum = a} :: NumericRangeFilter)

-- | Determines whether the minimum value in the filter value range should be
-- included in the filtered results.
numericRangeFilter_includeMinimum :: Lens.Lens' NumericRangeFilter (Prelude.Maybe Prelude.Bool)
numericRangeFilter_includeMinimum = Lens.lens (\NumericRangeFilter' {includeMinimum} -> includeMinimum) (\s@NumericRangeFilter' {} a -> s {includeMinimum = a} :: NumericRangeFilter)

-- | The maximum value for the filter value range.
numericRangeFilter_rangeMaximum :: Lens.Lens' NumericRangeFilter (Prelude.Maybe NumericRangeFilterValue)
numericRangeFilter_rangeMaximum = Lens.lens (\NumericRangeFilter' {rangeMaximum} -> rangeMaximum) (\s@NumericRangeFilter' {} a -> s {rangeMaximum = a} :: NumericRangeFilter)

-- | The minimum value for the filter value range.
numericRangeFilter_rangeMinimum :: Lens.Lens' NumericRangeFilter (Prelude.Maybe NumericRangeFilterValue)
numericRangeFilter_rangeMinimum = Lens.lens (\NumericRangeFilter' {rangeMinimum} -> rangeMinimum) (\s@NumericRangeFilter' {} a -> s {rangeMinimum = a} :: NumericRangeFilter)

-- | Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
numericRangeFilter_selectAllOptions :: Lens.Lens' NumericRangeFilter (Prelude.Maybe NumericFilterSelectAllOptions)
numericRangeFilter_selectAllOptions = Lens.lens (\NumericRangeFilter' {selectAllOptions} -> selectAllOptions) (\s@NumericRangeFilter' {} a -> s {selectAllOptions = a} :: NumericRangeFilter)

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
numericRangeFilter_filterId :: Lens.Lens' NumericRangeFilter Prelude.Text
numericRangeFilter_filterId = Lens.lens (\NumericRangeFilter' {filterId} -> filterId) (\s@NumericRangeFilter' {} a -> s {filterId = a} :: NumericRangeFilter)

-- | The column that the filter is applied to.
numericRangeFilter_column :: Lens.Lens' NumericRangeFilter ColumnIdentifier
numericRangeFilter_column = Lens.lens (\NumericRangeFilter' {column} -> column) (\s@NumericRangeFilter' {} a -> s {column = a} :: NumericRangeFilter)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
numericRangeFilter_nullOption :: Lens.Lens' NumericRangeFilter FilterNullOption
numericRangeFilter_nullOption = Lens.lens (\NumericRangeFilter' {nullOption} -> nullOption) (\s@NumericRangeFilter' {} a -> s {nullOption = a} :: NumericRangeFilter)

instance Data.FromJSON NumericRangeFilter where
  parseJSON =
    Data.withObject
      "NumericRangeFilter"
      ( \x ->
          NumericRangeFilter'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..:? "IncludeMaximum")
            Prelude.<*> (x Data..:? "IncludeMinimum")
            Prelude.<*> (x Data..:? "RangeMaximum")
            Prelude.<*> (x Data..:? "RangeMinimum")
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> (x Data..: "NullOption")
      )

instance Prelude.Hashable NumericRangeFilter where
  hashWithSalt _salt NumericRangeFilter' {..} =
    _salt `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` includeMaximum
      `Prelude.hashWithSalt` includeMinimum
      `Prelude.hashWithSalt` rangeMaximum
      `Prelude.hashWithSalt` rangeMinimum
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData NumericRangeFilter where
  rnf NumericRangeFilter' {..} =
    Prelude.rnf aggregationFunction
      `Prelude.seq` Prelude.rnf includeMaximum
      `Prelude.seq` Prelude.rnf includeMinimum
      `Prelude.seq` Prelude.rnf rangeMaximum
      `Prelude.seq` Prelude.rnf rangeMinimum
      `Prelude.seq` Prelude.rnf selectAllOptions
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf nullOption

instance Data.ToJSON NumericRangeFilter where
  toJSON NumericRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregationFunction" Data..=)
              Prelude.<$> aggregationFunction,
            ("IncludeMaximum" Data..=)
              Prelude.<$> includeMaximum,
            ("IncludeMinimum" Data..=)
              Prelude.<$> includeMinimum,
            ("RangeMaximum" Data..=) Prelude.<$> rangeMaximum,
            ("RangeMinimum" Data..=) Prelude.<$> rangeMinimum,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
