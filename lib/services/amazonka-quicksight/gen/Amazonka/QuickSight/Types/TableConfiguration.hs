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
-- Module      : Amazonka.QuickSight.Types.TableConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldOptions
import Amazonka.QuickSight.Types.TableFieldWells
import Amazonka.QuickSight.Types.TableOptions
import Amazonka.QuickSight.Types.TablePaginatedReportOptions
import Amazonka.QuickSight.Types.TableSortConfiguration
import Amazonka.QuickSight.Types.TotalOptions

-- | The configuration for a @TableVisual@.
--
-- /See:/ 'newTableConfiguration' smart constructor.
data TableConfiguration = TableConfiguration'
  { -- | The field options for a table visual.
    fieldOptions :: Prelude.Maybe TableFieldOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe TableFieldWells,
    -- | The paginated report options for a table visual.
    paginatedReportOptions :: Prelude.Maybe TablePaginatedReportOptions,
    -- | The sort configuration for a @TableVisual@.
    sortConfiguration :: Prelude.Maybe TableSortConfiguration,
    -- | The table options for a table visual.
    tableOptions :: Prelude.Maybe TableOptions,
    -- | The total options for a table visual.
    totalOptions :: Prelude.Maybe TotalOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldOptions', 'tableConfiguration_fieldOptions' - The field options for a table visual.
--
-- 'fieldWells', 'tableConfiguration_fieldWells' - The field wells of the visual.
--
-- 'paginatedReportOptions', 'tableConfiguration_paginatedReportOptions' - The paginated report options for a table visual.
--
-- 'sortConfiguration', 'tableConfiguration_sortConfiguration' - The sort configuration for a @TableVisual@.
--
-- 'tableOptions', 'tableConfiguration_tableOptions' - The table options for a table visual.
--
-- 'totalOptions', 'tableConfiguration_totalOptions' - The total options for a table visual.
newTableConfiguration ::
  TableConfiguration
newTableConfiguration =
  TableConfiguration'
    { fieldOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      paginatedReportOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tableOptions = Prelude.Nothing,
      totalOptions = Prelude.Nothing
    }

-- | The field options for a table visual.
tableConfiguration_fieldOptions :: Lens.Lens' TableConfiguration (Prelude.Maybe TableFieldOptions)
tableConfiguration_fieldOptions = Lens.lens (\TableConfiguration' {fieldOptions} -> fieldOptions) (\s@TableConfiguration' {} a -> s {fieldOptions = a} :: TableConfiguration)

-- | The field wells of the visual.
tableConfiguration_fieldWells :: Lens.Lens' TableConfiguration (Prelude.Maybe TableFieldWells)
tableConfiguration_fieldWells = Lens.lens (\TableConfiguration' {fieldWells} -> fieldWells) (\s@TableConfiguration' {} a -> s {fieldWells = a} :: TableConfiguration)

-- | The paginated report options for a table visual.
tableConfiguration_paginatedReportOptions :: Lens.Lens' TableConfiguration (Prelude.Maybe TablePaginatedReportOptions)
tableConfiguration_paginatedReportOptions = Lens.lens (\TableConfiguration' {paginatedReportOptions} -> paginatedReportOptions) (\s@TableConfiguration' {} a -> s {paginatedReportOptions = a} :: TableConfiguration)

-- | The sort configuration for a @TableVisual@.
tableConfiguration_sortConfiguration :: Lens.Lens' TableConfiguration (Prelude.Maybe TableSortConfiguration)
tableConfiguration_sortConfiguration = Lens.lens (\TableConfiguration' {sortConfiguration} -> sortConfiguration) (\s@TableConfiguration' {} a -> s {sortConfiguration = a} :: TableConfiguration)

-- | The table options for a table visual.
tableConfiguration_tableOptions :: Lens.Lens' TableConfiguration (Prelude.Maybe TableOptions)
tableConfiguration_tableOptions = Lens.lens (\TableConfiguration' {tableOptions} -> tableOptions) (\s@TableConfiguration' {} a -> s {tableOptions = a} :: TableConfiguration)

-- | The total options for a table visual.
tableConfiguration_totalOptions :: Lens.Lens' TableConfiguration (Prelude.Maybe TotalOptions)
tableConfiguration_totalOptions = Lens.lens (\TableConfiguration' {totalOptions} -> totalOptions) (\s@TableConfiguration' {} a -> s {totalOptions = a} :: TableConfiguration)

instance Data.FromJSON TableConfiguration where
  parseJSON =
    Data.withObject
      "TableConfiguration"
      ( \x ->
          TableConfiguration'
            Prelude.<$> (x Data..:? "FieldOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "PaginatedReportOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "TableOptions")
            Prelude.<*> (x Data..:? "TotalOptions")
      )

instance Prelude.Hashable TableConfiguration where
  hashWithSalt _salt TableConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fieldOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` paginatedReportOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tableOptions
      `Prelude.hashWithSalt` totalOptions

instance Prelude.NFData TableConfiguration where
  rnf TableConfiguration' {..} =
    Prelude.rnf fieldOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf paginatedReportOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tableOptions
      `Prelude.seq` Prelude.rnf totalOptions

instance Data.ToJSON TableConfiguration where
  toJSON TableConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldOptions" Data..=) Prelude.<$> fieldOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("PaginatedReportOptions" Data..=)
              Prelude.<$> paginatedReportOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("TableOptions" Data..=) Prelude.<$> tableOptions,
            ("TotalOptions" Data..=) Prelude.<$> totalOptions
          ]
      )
