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
-- Module      : Amazonka.QuickSight.Types.CategoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilterConfiguration
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | A @CategoryFilter@ filters text values.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/add-a-text-filter-data-prep.html Adding text filters>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newCategoryFilter' smart constructor.
data CategoryFilter = CategoryFilter'
  { -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The configuration for a @CategoryFilter@.
    configuration :: CategoryFilterConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterId', 'categoryFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'categoryFilter_column' - The column that the filter is applied to.
--
-- 'configuration', 'categoryFilter_configuration' - The configuration for a @CategoryFilter@.
newCategoryFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  -- | 'configuration'
  CategoryFilterConfiguration ->
  CategoryFilter
newCategoryFilter pFilterId_ pColumn_ pConfiguration_ =
  CategoryFilter'
    { filterId = pFilterId_,
      column = pColumn_,
      configuration = pConfiguration_
    }

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
categoryFilter_filterId :: Lens.Lens' CategoryFilter Prelude.Text
categoryFilter_filterId = Lens.lens (\CategoryFilter' {filterId} -> filterId) (\s@CategoryFilter' {} a -> s {filterId = a} :: CategoryFilter)

-- | The column that the filter is applied to.
categoryFilter_column :: Lens.Lens' CategoryFilter ColumnIdentifier
categoryFilter_column = Lens.lens (\CategoryFilter' {column} -> column) (\s@CategoryFilter' {} a -> s {column = a} :: CategoryFilter)

-- | The configuration for a @CategoryFilter@.
categoryFilter_configuration :: Lens.Lens' CategoryFilter CategoryFilterConfiguration
categoryFilter_configuration = Lens.lens (\CategoryFilter' {configuration} -> configuration) (\s@CategoryFilter' {} a -> s {configuration = a} :: CategoryFilter)

instance Data.FromJSON CategoryFilter where
  parseJSON =
    Data.withObject
      "CategoryFilter"
      ( \x ->
          CategoryFilter'
            Prelude.<$> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> (x Data..: "Configuration")
      )

instance Prelude.Hashable CategoryFilter where
  hashWithSalt _salt CategoryFilter' {..} =
    _salt
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData CategoryFilter where
  rnf CategoryFilter' {..} =
    Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToJSON CategoryFilter where
  toJSON CategoryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )
