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
-- Module      : Amazonka.QuickSight.Types.FilterListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilterMatchOperator
import Amazonka.QuickSight.Types.CategoryFilterSelectAllOptions

-- | A list of filter configurations.
--
-- /See:/ 'newFilterListConfiguration' smart constructor.
data FilterListConfiguration = FilterListConfiguration'
  { -- | The list of category values for the filter.
    categoryValues :: Prelude.Maybe [Prelude.Text],
    -- | Select all of the values. Null is not the assigned value of select all.
    --
    -- -   @FILTER_ALL_VALUES@
    selectAllOptions :: Prelude.Maybe CategoryFilterSelectAllOptions,
    -- | The match operator that is used to determine if a filter should be
    -- applied.
    matchOperator :: CategoryFilterMatchOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryValues', 'filterListConfiguration_categoryValues' - The list of category values for the filter.
--
-- 'selectAllOptions', 'filterListConfiguration_selectAllOptions' - Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
--
-- 'matchOperator', 'filterListConfiguration_matchOperator' - The match operator that is used to determine if a filter should be
-- applied.
newFilterListConfiguration ::
  -- | 'matchOperator'
  CategoryFilterMatchOperator ->
  FilterListConfiguration
newFilterListConfiguration pMatchOperator_ =
  FilterListConfiguration'
    { categoryValues =
        Prelude.Nothing,
      selectAllOptions = Prelude.Nothing,
      matchOperator = pMatchOperator_
    }

-- | The list of category values for the filter.
filterListConfiguration_categoryValues :: Lens.Lens' FilterListConfiguration (Prelude.Maybe [Prelude.Text])
filterListConfiguration_categoryValues = Lens.lens (\FilterListConfiguration' {categoryValues} -> categoryValues) (\s@FilterListConfiguration' {} a -> s {categoryValues = a} :: FilterListConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Select all of the values. Null is not the assigned value of select all.
--
-- -   @FILTER_ALL_VALUES@
filterListConfiguration_selectAllOptions :: Lens.Lens' FilterListConfiguration (Prelude.Maybe CategoryFilterSelectAllOptions)
filterListConfiguration_selectAllOptions = Lens.lens (\FilterListConfiguration' {selectAllOptions} -> selectAllOptions) (\s@FilterListConfiguration' {} a -> s {selectAllOptions = a} :: FilterListConfiguration)

-- | The match operator that is used to determine if a filter should be
-- applied.
filterListConfiguration_matchOperator :: Lens.Lens' FilterListConfiguration CategoryFilterMatchOperator
filterListConfiguration_matchOperator = Lens.lens (\FilterListConfiguration' {matchOperator} -> matchOperator) (\s@FilterListConfiguration' {} a -> s {matchOperator = a} :: FilterListConfiguration)

instance Data.FromJSON FilterListConfiguration where
  parseJSON =
    Data.withObject
      "FilterListConfiguration"
      ( \x ->
          FilterListConfiguration'
            Prelude.<$> (x Data..:? "CategoryValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..: "MatchOperator")
      )

instance Prelude.Hashable FilterListConfiguration where
  hashWithSalt _salt FilterListConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryValues
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` matchOperator

instance Prelude.NFData FilterListConfiguration where
  rnf FilterListConfiguration' {..} =
    Prelude.rnf categoryValues
      `Prelude.seq` Prelude.rnf selectAllOptions
      `Prelude.seq` Prelude.rnf matchOperator

instance Data.ToJSON FilterListConfiguration where
  toJSON FilterListConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryValues" Data..=)
              Prelude.<$> categoryValues,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            Prelude.Just
              ("MatchOperator" Data..= matchOperator)
          ]
      )
