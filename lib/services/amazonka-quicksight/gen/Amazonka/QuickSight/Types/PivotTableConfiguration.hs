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
-- Module      : Amazonka.QuickSight.Types.PivotTableConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableFieldOptions
import Amazonka.QuickSight.Types.PivotTableFieldWells
import Amazonka.QuickSight.Types.PivotTableOptions
import Amazonka.QuickSight.Types.PivotTablePaginatedReportOptions
import Amazonka.QuickSight.Types.PivotTableSortConfiguration
import Amazonka.QuickSight.Types.PivotTableTotalOptions

-- | The configuration for a @PivotTableVisual@.
--
-- /See:/ 'newPivotTableConfiguration' smart constructor.
data PivotTableConfiguration = PivotTableConfiguration'
  { -- | The field options for a pivot table visual.
    fieldOptions :: Prelude.Maybe PivotTableFieldOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe PivotTableFieldWells,
    -- | The paginated report options for a pivot table visual.
    paginatedReportOptions :: Prelude.Maybe PivotTablePaginatedReportOptions,
    -- | The sort configuration for a @PivotTableVisual@.
    sortConfiguration :: Prelude.Maybe PivotTableSortConfiguration,
    -- | The table options for a pivot table visual.
    tableOptions :: Prelude.Maybe PivotTableOptions,
    -- | The total options for a pivot table visual.
    totalOptions :: Prelude.Maybe PivotTableTotalOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldOptions', 'pivotTableConfiguration_fieldOptions' - The field options for a pivot table visual.
--
-- 'fieldWells', 'pivotTableConfiguration_fieldWells' - The field wells of the visual.
--
-- 'paginatedReportOptions', 'pivotTableConfiguration_paginatedReportOptions' - The paginated report options for a pivot table visual.
--
-- 'sortConfiguration', 'pivotTableConfiguration_sortConfiguration' - The sort configuration for a @PivotTableVisual@.
--
-- 'tableOptions', 'pivotTableConfiguration_tableOptions' - The table options for a pivot table visual.
--
-- 'totalOptions', 'pivotTableConfiguration_totalOptions' - The total options for a pivot table visual.
newPivotTableConfiguration ::
  PivotTableConfiguration
newPivotTableConfiguration =
  PivotTableConfiguration'
    { fieldOptions =
        Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      paginatedReportOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tableOptions = Prelude.Nothing,
      totalOptions = Prelude.Nothing
    }

-- | The field options for a pivot table visual.
pivotTableConfiguration_fieldOptions :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTableFieldOptions)
pivotTableConfiguration_fieldOptions = Lens.lens (\PivotTableConfiguration' {fieldOptions} -> fieldOptions) (\s@PivotTableConfiguration' {} a -> s {fieldOptions = a} :: PivotTableConfiguration)

-- | The field wells of the visual.
pivotTableConfiguration_fieldWells :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTableFieldWells)
pivotTableConfiguration_fieldWells = Lens.lens (\PivotTableConfiguration' {fieldWells} -> fieldWells) (\s@PivotTableConfiguration' {} a -> s {fieldWells = a} :: PivotTableConfiguration)

-- | The paginated report options for a pivot table visual.
pivotTableConfiguration_paginatedReportOptions :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTablePaginatedReportOptions)
pivotTableConfiguration_paginatedReportOptions = Lens.lens (\PivotTableConfiguration' {paginatedReportOptions} -> paginatedReportOptions) (\s@PivotTableConfiguration' {} a -> s {paginatedReportOptions = a} :: PivotTableConfiguration)

-- | The sort configuration for a @PivotTableVisual@.
pivotTableConfiguration_sortConfiguration :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTableSortConfiguration)
pivotTableConfiguration_sortConfiguration = Lens.lens (\PivotTableConfiguration' {sortConfiguration} -> sortConfiguration) (\s@PivotTableConfiguration' {} a -> s {sortConfiguration = a} :: PivotTableConfiguration)

-- | The table options for a pivot table visual.
pivotTableConfiguration_tableOptions :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTableOptions)
pivotTableConfiguration_tableOptions = Lens.lens (\PivotTableConfiguration' {tableOptions} -> tableOptions) (\s@PivotTableConfiguration' {} a -> s {tableOptions = a} :: PivotTableConfiguration)

-- | The total options for a pivot table visual.
pivotTableConfiguration_totalOptions :: Lens.Lens' PivotTableConfiguration (Prelude.Maybe PivotTableTotalOptions)
pivotTableConfiguration_totalOptions = Lens.lens (\PivotTableConfiguration' {totalOptions} -> totalOptions) (\s@PivotTableConfiguration' {} a -> s {totalOptions = a} :: PivotTableConfiguration)

instance Data.FromJSON PivotTableConfiguration where
  parseJSON =
    Data.withObject
      "PivotTableConfiguration"
      ( \x ->
          PivotTableConfiguration'
            Prelude.<$> (x Data..:? "FieldOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "PaginatedReportOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "TableOptions")
            Prelude.<*> (x Data..:? "TotalOptions")
      )

instance Prelude.Hashable PivotTableConfiguration where
  hashWithSalt _salt PivotTableConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fieldOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` paginatedReportOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tableOptions
      `Prelude.hashWithSalt` totalOptions

instance Prelude.NFData PivotTableConfiguration where
  rnf PivotTableConfiguration' {..} =
    Prelude.rnf fieldOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf paginatedReportOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tableOptions
      `Prelude.seq` Prelude.rnf totalOptions

instance Data.ToJSON PivotTableConfiguration where
  toJSON PivotTableConfiguration' {..} =
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
