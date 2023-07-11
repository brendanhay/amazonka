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
-- Module      : Amazonka.QuickSight.Types.BoxPlotSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.PaginationConfiguration

-- | The sort configuration of a @BoxPlotVisual@.
--
-- /See:/ 'newBoxPlotSortConfiguration' smart constructor.
data BoxPlotSortConfiguration = BoxPlotSortConfiguration'
  { -- | The sort configuration of a group by fields.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The pagination configuration of a table visual or box plot.
    paginationConfiguration :: Prelude.Maybe PaginationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categorySort', 'boxPlotSortConfiguration_categorySort' - The sort configuration of a group by fields.
--
-- 'paginationConfiguration', 'boxPlotSortConfiguration_paginationConfiguration' - The pagination configuration of a table visual or box plot.
newBoxPlotSortConfiguration ::
  BoxPlotSortConfiguration
newBoxPlotSortConfiguration =
  BoxPlotSortConfiguration'
    { categorySort =
        Prelude.Nothing,
      paginationConfiguration = Prelude.Nothing
    }

-- | The sort configuration of a group by fields.
boxPlotSortConfiguration_categorySort :: Lens.Lens' BoxPlotSortConfiguration (Prelude.Maybe [FieldSortOptions])
boxPlotSortConfiguration_categorySort = Lens.lens (\BoxPlotSortConfiguration' {categorySort} -> categorySort) (\s@BoxPlotSortConfiguration' {} a -> s {categorySort = a} :: BoxPlotSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The pagination configuration of a table visual or box plot.
boxPlotSortConfiguration_paginationConfiguration :: Lens.Lens' BoxPlotSortConfiguration (Prelude.Maybe PaginationConfiguration)
boxPlotSortConfiguration_paginationConfiguration = Lens.lens (\BoxPlotSortConfiguration' {paginationConfiguration} -> paginationConfiguration) (\s@BoxPlotSortConfiguration' {} a -> s {paginationConfiguration = a} :: BoxPlotSortConfiguration)

instance Data.FromJSON BoxPlotSortConfiguration where
  parseJSON =
    Data.withObject
      "BoxPlotSortConfiguration"
      ( \x ->
          BoxPlotSortConfiguration'
            Prelude.<$> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PaginationConfiguration")
      )

instance Prelude.Hashable BoxPlotSortConfiguration where
  hashWithSalt _salt BoxPlotSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` paginationConfiguration

instance Prelude.NFData BoxPlotSortConfiguration where
  rnf BoxPlotSortConfiguration' {..} =
    Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf paginationConfiguration

instance Data.ToJSON BoxPlotSortConfiguration where
  toJSON BoxPlotSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategorySort" Data..=) Prelude.<$> categorySort,
            ("PaginationConfiguration" Data..=)
              Prelude.<$> paginationConfiguration
          ]
      )
