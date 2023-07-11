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
-- Module      : Amazonka.QuickSight.Types.NumericEqualityFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericEqualityFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.FilterNullOption
import Amazonka.QuickSight.Types.NumericEqualityMatchOperator
import Amazonka.QuickSight.Types.NumericFilterSelectAllOptions

-- | A @NumericEqualityFilter@ filters values that are equal to the specified
-- value.
--
-- /See:/ 'newNumericEqualityFilter' smart constructor.
data NumericEqualityFilter = NumericEqualityFilter'
  { -- | The aggregation function of the filter.
    aggregationFunction :: Prelude.Maybe AggregationFunction,
    -- | The parameter whose value should be used for the filter value.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Select all of the values. Null is not the assigned value of select all.
    --
    -- -   @FILTER_ALL_VALUES@
    selectAllOptions :: Prelude.Maybe NumericFilterSelectAllOptions,
    -- | The input value.
    value :: Prelude.Maybe Prelude.Double,
    -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The match operator that is used to determine if a filter should be
    -- applied.
    matchOperator :: NumericEqualityMatchOperator,
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
-- Create a value of 'NumericEqualityFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'numericEqualityFilter_aggregationFunction' - The aggregation function of the filter.
--
-- 'parameterName', 'numericEqualityFilter_parameterName' - The parameter whose value should be used for the filter value.
--
-- 'selectAllOptions', 'numericEqualityFilter_selectAllOptions' - Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
--
-- 'value', 'numericEqualityFilter_value' - The input value.
--
-- 'filterId', 'numericEqualityFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'numericEqualityFilter_column' - The column that the filter is applied to.
--
-- 'matchOperator', 'numericEqualityFilter_matchOperator' - The match operator that is used to determine if a filter should be
-- applied.
--
-- 'nullOption', 'numericEqualityFilter_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newNumericEqualityFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  -- | 'matchOperator'
  NumericEqualityMatchOperator ->
  -- | 'nullOption'
  FilterNullOption ->
  NumericEqualityFilter
newNumericEqualityFilter
  pFilterId_
  pColumn_
  pMatchOperator_
  pNullOption_ =
    NumericEqualityFilter'
      { aggregationFunction =
          Prelude.Nothing,
        parameterName = Prelude.Nothing,
        selectAllOptions = Prelude.Nothing,
        value = Prelude.Nothing,
        filterId = pFilterId_,
        column = pColumn_,
        matchOperator = pMatchOperator_,
        nullOption = pNullOption_
      }

-- | The aggregation function of the filter.
numericEqualityFilter_aggregationFunction :: Lens.Lens' NumericEqualityFilter (Prelude.Maybe AggregationFunction)
numericEqualityFilter_aggregationFunction = Lens.lens (\NumericEqualityFilter' {aggregationFunction} -> aggregationFunction) (\s@NumericEqualityFilter' {} a -> s {aggregationFunction = a} :: NumericEqualityFilter)

-- | The parameter whose value should be used for the filter value.
numericEqualityFilter_parameterName :: Lens.Lens' NumericEqualityFilter (Prelude.Maybe Prelude.Text)
numericEqualityFilter_parameterName = Lens.lens (\NumericEqualityFilter' {parameterName} -> parameterName) (\s@NumericEqualityFilter' {} a -> s {parameterName = a} :: NumericEqualityFilter)

-- | Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
numericEqualityFilter_selectAllOptions :: Lens.Lens' NumericEqualityFilter (Prelude.Maybe NumericFilterSelectAllOptions)
numericEqualityFilter_selectAllOptions = Lens.lens (\NumericEqualityFilter' {selectAllOptions} -> selectAllOptions) (\s@NumericEqualityFilter' {} a -> s {selectAllOptions = a} :: NumericEqualityFilter)

-- | The input value.
numericEqualityFilter_value :: Lens.Lens' NumericEqualityFilter (Prelude.Maybe Prelude.Double)
numericEqualityFilter_value = Lens.lens (\NumericEqualityFilter' {value} -> value) (\s@NumericEqualityFilter' {} a -> s {value = a} :: NumericEqualityFilter)

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
numericEqualityFilter_filterId :: Lens.Lens' NumericEqualityFilter Prelude.Text
numericEqualityFilter_filterId = Lens.lens (\NumericEqualityFilter' {filterId} -> filterId) (\s@NumericEqualityFilter' {} a -> s {filterId = a} :: NumericEqualityFilter)

-- | The column that the filter is applied to.
numericEqualityFilter_column :: Lens.Lens' NumericEqualityFilter ColumnIdentifier
numericEqualityFilter_column = Lens.lens (\NumericEqualityFilter' {column} -> column) (\s@NumericEqualityFilter' {} a -> s {column = a} :: NumericEqualityFilter)

-- | The match operator that is used to determine if a filter should be
-- applied.
numericEqualityFilter_matchOperator :: Lens.Lens' NumericEqualityFilter NumericEqualityMatchOperator
numericEqualityFilter_matchOperator = Lens.lens (\NumericEqualityFilter' {matchOperator} -> matchOperator) (\s@NumericEqualityFilter' {} a -> s {matchOperator = a} :: NumericEqualityFilter)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
numericEqualityFilter_nullOption :: Lens.Lens' NumericEqualityFilter FilterNullOption
numericEqualityFilter_nullOption = Lens.lens (\NumericEqualityFilter' {nullOption} -> nullOption) (\s@NumericEqualityFilter' {} a -> s {nullOption = a} :: NumericEqualityFilter)

instance Data.FromJSON NumericEqualityFilter where
  parseJSON =
    Data.withObject
      "NumericEqualityFilter"
      ( \x ->
          NumericEqualityFilter'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> (x Data..: "MatchOperator")
            Prelude.<*> (x Data..: "NullOption")
      )

instance Prelude.Hashable NumericEqualityFilter where
  hashWithSalt _salt NumericEqualityFilter' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` matchOperator
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData NumericEqualityFilter where
  rnf NumericEqualityFilter' {..} =
    Prelude.rnf aggregationFunction
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf selectAllOptions
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf matchOperator
      `Prelude.seq` Prelude.rnf nullOption

instance Data.ToJSON NumericEqualityFilter where
  toJSON NumericEqualityFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregationFunction" Data..=)
              Prelude.<$> aggregationFunction,
            ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just ("MatchOperator" Data..= matchOperator),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
