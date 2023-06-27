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
-- Module      : Amazonka.QuickSight.Types.CustomFilterConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomFilterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilterMatchOperator
import Amazonka.QuickSight.Types.CategoryFilterSelectAllOptions
import Amazonka.QuickSight.Types.FilterNullOption

-- | A custom filter that filters based on a single value. This filter can be
-- partially matched.
--
-- /See:/ 'newCustomFilterConfiguration' smart constructor.
data CustomFilterConfiguration = CustomFilterConfiguration'
  { -- | The category value for the filter.
    --
    -- This field is mutually exclusive to @ParameterName@.
    categoryValue :: Prelude.Maybe Prelude.Text,
    -- | The parameter whose value should be used for the filter value.
    --
    -- This field is mutually exclusive to @CategoryValue@.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Select all of the values. Null is not the assigned value of select all.
    --
    -- -   @FILTER_ALL_VALUES@
    selectAllOptions :: Prelude.Maybe CategoryFilterSelectAllOptions,
    -- | The match operator that is used to determine if a filter should be
    -- applied.
    matchOperator :: CategoryFilterMatchOperator,
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
-- Create a value of 'CustomFilterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryValue', 'customFilterConfiguration_categoryValue' - The category value for the filter.
--
-- This field is mutually exclusive to @ParameterName@.
--
-- 'parameterName', 'customFilterConfiguration_parameterName' - The parameter whose value should be used for the filter value.
--
-- This field is mutually exclusive to @CategoryValue@.
--
-- 'selectAllOptions', 'customFilterConfiguration_selectAllOptions' - Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
--
-- 'matchOperator', 'customFilterConfiguration_matchOperator' - The match operator that is used to determine if a filter should be
-- applied.
--
-- 'nullOption', 'customFilterConfiguration_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newCustomFilterConfiguration ::
  -- | 'matchOperator'
  CategoryFilterMatchOperator ->
  -- | 'nullOption'
  FilterNullOption ->
  CustomFilterConfiguration
newCustomFilterConfiguration
  pMatchOperator_
  pNullOption_ =
    CustomFilterConfiguration'
      { categoryValue =
          Prelude.Nothing,
        parameterName = Prelude.Nothing,
        selectAllOptions = Prelude.Nothing,
        matchOperator = pMatchOperator_,
        nullOption = pNullOption_
      }

-- | The category value for the filter.
--
-- This field is mutually exclusive to @ParameterName@.
customFilterConfiguration_categoryValue :: Lens.Lens' CustomFilterConfiguration (Prelude.Maybe Prelude.Text)
customFilterConfiguration_categoryValue = Lens.lens (\CustomFilterConfiguration' {categoryValue} -> categoryValue) (\s@CustomFilterConfiguration' {} a -> s {categoryValue = a} :: CustomFilterConfiguration)

-- | The parameter whose value should be used for the filter value.
--
-- This field is mutually exclusive to @CategoryValue@.
customFilterConfiguration_parameterName :: Lens.Lens' CustomFilterConfiguration (Prelude.Maybe Prelude.Text)
customFilterConfiguration_parameterName = Lens.lens (\CustomFilterConfiguration' {parameterName} -> parameterName) (\s@CustomFilterConfiguration' {} a -> s {parameterName = a} :: CustomFilterConfiguration)

-- | Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
customFilterConfiguration_selectAllOptions :: Lens.Lens' CustomFilterConfiguration (Prelude.Maybe CategoryFilterSelectAllOptions)
customFilterConfiguration_selectAllOptions = Lens.lens (\CustomFilterConfiguration' {selectAllOptions} -> selectAllOptions) (\s@CustomFilterConfiguration' {} a -> s {selectAllOptions = a} :: CustomFilterConfiguration)

-- | The match operator that is used to determine if a filter should be
-- applied.
customFilterConfiguration_matchOperator :: Lens.Lens' CustomFilterConfiguration CategoryFilterMatchOperator
customFilterConfiguration_matchOperator = Lens.lens (\CustomFilterConfiguration' {matchOperator} -> matchOperator) (\s@CustomFilterConfiguration' {} a -> s {matchOperator = a} :: CustomFilterConfiguration)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
customFilterConfiguration_nullOption :: Lens.Lens' CustomFilterConfiguration FilterNullOption
customFilterConfiguration_nullOption = Lens.lens (\CustomFilterConfiguration' {nullOption} -> nullOption) (\s@CustomFilterConfiguration' {} a -> s {nullOption = a} :: CustomFilterConfiguration)

instance Data.FromJSON CustomFilterConfiguration where
  parseJSON =
    Data.withObject
      "CustomFilterConfiguration"
      ( \x ->
          CustomFilterConfiguration'
            Prelude.<$> (x Data..:? "CategoryValue")
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..: "MatchOperator")
            Prelude.<*> (x Data..: "NullOption")
      )

instance Prelude.Hashable CustomFilterConfiguration where
  hashWithSalt _salt CustomFilterConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryValue
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` matchOperator
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData CustomFilterConfiguration where
  rnf CustomFilterConfiguration' {..} =
    Prelude.rnf categoryValue
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf selectAllOptions
      `Prelude.seq` Prelude.rnf matchOperator
      `Prelude.seq` Prelude.rnf nullOption

instance Data.ToJSON CustomFilterConfiguration where
  toJSON CustomFilterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryValue" Data..=) Prelude.<$> categoryValue,
            ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            Prelude.Just ("MatchOperator" Data..= matchOperator),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
