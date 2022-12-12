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
-- Module      : Amazonka.QuickSight.Types.TableSortConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.PaginationConfiguration

-- | The sort configuration for a @TableVisual@.
--
-- /See:/ 'newTableSortConfiguration' smart constructor.
data TableSortConfiguration = TableSortConfiguration'
  { -- | The pagination configuration (page size, page number) for the table.
    paginationConfiguration :: Prelude.Maybe PaginationConfiguration,
    -- | The field sort options for rows in the table.
    rowSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationConfiguration', 'tableSortConfiguration_paginationConfiguration' - The pagination configuration (page size, page number) for the table.
--
-- 'rowSort', 'tableSortConfiguration_rowSort' - The field sort options for rows in the table.
newTableSortConfiguration ::
  TableSortConfiguration
newTableSortConfiguration =
  TableSortConfiguration'
    { paginationConfiguration =
        Prelude.Nothing,
      rowSort = Prelude.Nothing
    }

-- | The pagination configuration (page size, page number) for the table.
tableSortConfiguration_paginationConfiguration :: Lens.Lens' TableSortConfiguration (Prelude.Maybe PaginationConfiguration)
tableSortConfiguration_paginationConfiguration = Lens.lens (\TableSortConfiguration' {paginationConfiguration} -> paginationConfiguration) (\s@TableSortConfiguration' {} a -> s {paginationConfiguration = a} :: TableSortConfiguration)

-- | The field sort options for rows in the table.
tableSortConfiguration_rowSort :: Lens.Lens' TableSortConfiguration (Prelude.Maybe [FieldSortOptions])
tableSortConfiguration_rowSort = Lens.lens (\TableSortConfiguration' {rowSort} -> rowSort) (\s@TableSortConfiguration' {} a -> s {rowSort = a} :: TableSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableSortConfiguration where
  parseJSON =
    Data.withObject
      "TableSortConfiguration"
      ( \x ->
          TableSortConfiguration'
            Prelude.<$> (x Data..:? "PaginationConfiguration")
            Prelude.<*> (x Data..:? "RowSort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TableSortConfiguration where
  hashWithSalt _salt TableSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` paginationConfiguration
      `Prelude.hashWithSalt` rowSort

instance Prelude.NFData TableSortConfiguration where
  rnf TableSortConfiguration' {..} =
    Prelude.rnf paginationConfiguration
      `Prelude.seq` Prelude.rnf rowSort

instance Data.ToJSON TableSortConfiguration where
  toJSON TableSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PaginationConfiguration" Data..=)
              Prelude.<$> paginationConfiguration,
            ("RowSort" Data..=) Prelude.<$> rowSort
          ]
      )
