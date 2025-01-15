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
-- Module      : Amazonka.QuickSight.Types.CustomFilterListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomFilterListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilterMatchOperator
import Amazonka.QuickSight.Types.CategoryFilterSelectAllOptions
import Amazonka.QuickSight.Types.FilterNullOption

-- | A list of custom filter values.
--
-- /See:/ 'newCustomFilterListConfiguration' smart constructor.
data CustomFilterListConfiguration = CustomFilterListConfiguration'
  { -- | The list of category values for the filter.
    categoryValues :: Prelude.Maybe [Prelude.Text],
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
-- Create a value of 'CustomFilterListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryValues', 'customFilterListConfiguration_categoryValues' - The list of category values for the filter.
--
-- 'selectAllOptions', 'customFilterListConfiguration_selectAllOptions' - Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
--
-- 'matchOperator', 'customFilterListConfiguration_matchOperator' - The match operator that is used to determine if a filter should be
-- applied.
--
-- 'nullOption', 'customFilterListConfiguration_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newCustomFilterListConfiguration ::
  -- | 'matchOperator'
  CategoryFilterMatchOperator ->
  -- | 'nullOption'
  FilterNullOption ->
  CustomFilterListConfiguration
newCustomFilterListConfiguration
  pMatchOperator_
  pNullOption_ =
    CustomFilterListConfiguration'
      { categoryValues =
          Prelude.Nothing,
        selectAllOptions = Prelude.Nothing,
        matchOperator = pMatchOperator_,
        nullOption = pNullOption_
      }

-- | The list of category values for the filter.
customFilterListConfiguration_categoryValues :: Lens.Lens' CustomFilterListConfiguration (Prelude.Maybe [Prelude.Text])
customFilterListConfiguration_categoryValues = Lens.lens (\CustomFilterListConfiguration' {categoryValues} -> categoryValues) (\s@CustomFilterListConfiguration' {} a -> s {categoryValues = a} :: CustomFilterListConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
customFilterListConfiguration_selectAllOptions :: Lens.Lens' CustomFilterListConfiguration (Prelude.Maybe CategoryFilterSelectAllOptions)
customFilterListConfiguration_selectAllOptions = Lens.lens (\CustomFilterListConfiguration' {selectAllOptions} -> selectAllOptions) (\s@CustomFilterListConfiguration' {} a -> s {selectAllOptions = a} :: CustomFilterListConfiguration)

-- | The match operator that is used to determine if a filter should be
-- applied.
customFilterListConfiguration_matchOperator :: Lens.Lens' CustomFilterListConfiguration CategoryFilterMatchOperator
customFilterListConfiguration_matchOperator = Lens.lens (\CustomFilterListConfiguration' {matchOperator} -> matchOperator) (\s@CustomFilterListConfiguration' {} a -> s {matchOperator = a} :: CustomFilterListConfiguration)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
customFilterListConfiguration_nullOption :: Lens.Lens' CustomFilterListConfiguration FilterNullOption
customFilterListConfiguration_nullOption = Lens.lens (\CustomFilterListConfiguration' {nullOption} -> nullOption) (\s@CustomFilterListConfiguration' {} a -> s {nullOption = a} :: CustomFilterListConfiguration)

instance Data.FromJSON CustomFilterListConfiguration where
  parseJSON =
    Data.withObject
      "CustomFilterListConfiguration"
      ( \x ->
          CustomFilterListConfiguration'
            Prelude.<$> (x Data..:? "CategoryValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..: "MatchOperator")
            Prelude.<*> (x Data..: "NullOption")
      )

instance
  Prelude.Hashable
    CustomFilterListConfiguration
  where
  hashWithSalt _salt CustomFilterListConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryValues
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` matchOperator
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData CustomFilterListConfiguration where
  rnf CustomFilterListConfiguration' {..} =
    Prelude.rnf categoryValues `Prelude.seq`
      Prelude.rnf selectAllOptions `Prelude.seq`
        Prelude.rnf matchOperator `Prelude.seq`
          Prelude.rnf nullOption

instance Data.ToJSON CustomFilterListConfiguration where
  toJSON CustomFilterListConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryValues" Data..=)
              Prelude.<$> categoryValues,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            Prelude.Just ("MatchOperator" Data..= matchOperator),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
