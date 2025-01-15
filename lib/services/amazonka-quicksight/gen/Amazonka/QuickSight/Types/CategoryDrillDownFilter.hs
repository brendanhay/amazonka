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
-- Module      : Amazonka.QuickSight.Types.CategoryDrillDownFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoryDrillDownFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | The numeric equality type drill down filter.
--
-- /See:/ 'newCategoryDrillDownFilter' smart constructor.
data CategoryDrillDownFilter = CategoryDrillDownFilter'
  { -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | A list of the string inputs that are the values of the category drill
    -- down filter.
    categoryValues :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoryDrillDownFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'categoryDrillDownFilter_column' - The column that the filter is applied to.
--
-- 'categoryValues', 'categoryDrillDownFilter_categoryValues' - A list of the string inputs that are the values of the category drill
-- down filter.
newCategoryDrillDownFilter ::
  -- | 'column'
  ColumnIdentifier ->
  CategoryDrillDownFilter
newCategoryDrillDownFilter pColumn_ =
  CategoryDrillDownFilter'
    { column = pColumn_,
      categoryValues = Prelude.mempty
    }

-- | The column that the filter is applied to.
categoryDrillDownFilter_column :: Lens.Lens' CategoryDrillDownFilter ColumnIdentifier
categoryDrillDownFilter_column = Lens.lens (\CategoryDrillDownFilter' {column} -> column) (\s@CategoryDrillDownFilter' {} a -> s {column = a} :: CategoryDrillDownFilter)

-- | A list of the string inputs that are the values of the category drill
-- down filter.
categoryDrillDownFilter_categoryValues :: Lens.Lens' CategoryDrillDownFilter [Prelude.Text]
categoryDrillDownFilter_categoryValues = Lens.lens (\CategoryDrillDownFilter' {categoryValues} -> categoryValues) (\s@CategoryDrillDownFilter' {} a -> s {categoryValues = a} :: CategoryDrillDownFilter) Prelude.. Lens.coerced

instance Data.FromJSON CategoryDrillDownFilter where
  parseJSON =
    Data.withObject
      "CategoryDrillDownFilter"
      ( \x ->
          CategoryDrillDownFilter'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> ( x
                            Data..:? "CategoryValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CategoryDrillDownFilter where
  hashWithSalt _salt CategoryDrillDownFilter' {..} =
    _salt
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` categoryValues

instance Prelude.NFData CategoryDrillDownFilter where
  rnf CategoryDrillDownFilter' {..} =
    Prelude.rnf column `Prelude.seq`
      Prelude.rnf categoryValues

instance Data.ToJSON CategoryDrillDownFilter where
  toJSON CategoryDrillDownFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just
              ("CategoryValues" Data..= categoryValues)
          ]
      )
