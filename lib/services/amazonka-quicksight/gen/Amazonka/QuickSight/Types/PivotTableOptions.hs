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
-- Module      : Amazonka.QuickSight.Types.PivotTableOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableMetricPlacement
import Amazonka.QuickSight.Types.RowAlternateColorOptions
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.Visibility

-- | The table options for a pivot table visual.
--
-- /See:/ 'newPivotTableOptions' smart constructor.
data PivotTableOptions = PivotTableOptions'
  { -- | The table cell style of cells.
    cellStyle :: Prelude.Maybe TableCellStyle,
    -- | The table cell style of the column header.
    columnHeaderStyle :: Prelude.Maybe TableCellStyle,
    -- | The visibility of the column names.
    columnNamesVisibility :: Prelude.Maybe Visibility,
    -- | The metric placement (row, column) options.
    metricPlacement :: Prelude.Maybe PivotTableMetricPlacement,
    -- | The row alternate color options (widget status, row alternate colors).
    rowAlternateColorOptions :: Prelude.Maybe RowAlternateColorOptions,
    -- | The table cell style of row field names.
    rowFieldNamesStyle :: Prelude.Maybe TableCellStyle,
    -- | The table cell style of the row headers.
    rowHeaderStyle :: Prelude.Maybe TableCellStyle,
    -- | The visibility of the single metric options.
    singleMetricVisibility :: Prelude.Maybe Visibility,
    -- | Determines the visibility of the pivot table.
    toggleButtonsVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellStyle', 'pivotTableOptions_cellStyle' - The table cell style of cells.
--
-- 'columnHeaderStyle', 'pivotTableOptions_columnHeaderStyle' - The table cell style of the column header.
--
-- 'columnNamesVisibility', 'pivotTableOptions_columnNamesVisibility' - The visibility of the column names.
--
-- 'metricPlacement', 'pivotTableOptions_metricPlacement' - The metric placement (row, column) options.
--
-- 'rowAlternateColorOptions', 'pivotTableOptions_rowAlternateColorOptions' - The row alternate color options (widget status, row alternate colors).
--
-- 'rowFieldNamesStyle', 'pivotTableOptions_rowFieldNamesStyle' - The table cell style of row field names.
--
-- 'rowHeaderStyle', 'pivotTableOptions_rowHeaderStyle' - The table cell style of the row headers.
--
-- 'singleMetricVisibility', 'pivotTableOptions_singleMetricVisibility' - The visibility of the single metric options.
--
-- 'toggleButtonsVisibility', 'pivotTableOptions_toggleButtonsVisibility' - Determines the visibility of the pivot table.
newPivotTableOptions ::
  PivotTableOptions
newPivotTableOptions =
  PivotTableOptions'
    { cellStyle = Prelude.Nothing,
      columnHeaderStyle = Prelude.Nothing,
      columnNamesVisibility = Prelude.Nothing,
      metricPlacement = Prelude.Nothing,
      rowAlternateColorOptions = Prelude.Nothing,
      rowFieldNamesStyle = Prelude.Nothing,
      rowHeaderStyle = Prelude.Nothing,
      singleMetricVisibility = Prelude.Nothing,
      toggleButtonsVisibility = Prelude.Nothing
    }

-- | The table cell style of cells.
pivotTableOptions_cellStyle :: Lens.Lens' PivotTableOptions (Prelude.Maybe TableCellStyle)
pivotTableOptions_cellStyle = Lens.lens (\PivotTableOptions' {cellStyle} -> cellStyle) (\s@PivotTableOptions' {} a -> s {cellStyle = a} :: PivotTableOptions)

-- | The table cell style of the column header.
pivotTableOptions_columnHeaderStyle :: Lens.Lens' PivotTableOptions (Prelude.Maybe TableCellStyle)
pivotTableOptions_columnHeaderStyle = Lens.lens (\PivotTableOptions' {columnHeaderStyle} -> columnHeaderStyle) (\s@PivotTableOptions' {} a -> s {columnHeaderStyle = a} :: PivotTableOptions)

-- | The visibility of the column names.
pivotTableOptions_columnNamesVisibility :: Lens.Lens' PivotTableOptions (Prelude.Maybe Visibility)
pivotTableOptions_columnNamesVisibility = Lens.lens (\PivotTableOptions' {columnNamesVisibility} -> columnNamesVisibility) (\s@PivotTableOptions' {} a -> s {columnNamesVisibility = a} :: PivotTableOptions)

-- | The metric placement (row, column) options.
pivotTableOptions_metricPlacement :: Lens.Lens' PivotTableOptions (Prelude.Maybe PivotTableMetricPlacement)
pivotTableOptions_metricPlacement = Lens.lens (\PivotTableOptions' {metricPlacement} -> metricPlacement) (\s@PivotTableOptions' {} a -> s {metricPlacement = a} :: PivotTableOptions)

-- | The row alternate color options (widget status, row alternate colors).
pivotTableOptions_rowAlternateColorOptions :: Lens.Lens' PivotTableOptions (Prelude.Maybe RowAlternateColorOptions)
pivotTableOptions_rowAlternateColorOptions = Lens.lens (\PivotTableOptions' {rowAlternateColorOptions} -> rowAlternateColorOptions) (\s@PivotTableOptions' {} a -> s {rowAlternateColorOptions = a} :: PivotTableOptions)

-- | The table cell style of row field names.
pivotTableOptions_rowFieldNamesStyle :: Lens.Lens' PivotTableOptions (Prelude.Maybe TableCellStyle)
pivotTableOptions_rowFieldNamesStyle = Lens.lens (\PivotTableOptions' {rowFieldNamesStyle} -> rowFieldNamesStyle) (\s@PivotTableOptions' {} a -> s {rowFieldNamesStyle = a} :: PivotTableOptions)

-- | The table cell style of the row headers.
pivotTableOptions_rowHeaderStyle :: Lens.Lens' PivotTableOptions (Prelude.Maybe TableCellStyle)
pivotTableOptions_rowHeaderStyle = Lens.lens (\PivotTableOptions' {rowHeaderStyle} -> rowHeaderStyle) (\s@PivotTableOptions' {} a -> s {rowHeaderStyle = a} :: PivotTableOptions)

-- | The visibility of the single metric options.
pivotTableOptions_singleMetricVisibility :: Lens.Lens' PivotTableOptions (Prelude.Maybe Visibility)
pivotTableOptions_singleMetricVisibility = Lens.lens (\PivotTableOptions' {singleMetricVisibility} -> singleMetricVisibility) (\s@PivotTableOptions' {} a -> s {singleMetricVisibility = a} :: PivotTableOptions)

-- | Determines the visibility of the pivot table.
pivotTableOptions_toggleButtonsVisibility :: Lens.Lens' PivotTableOptions (Prelude.Maybe Visibility)
pivotTableOptions_toggleButtonsVisibility = Lens.lens (\PivotTableOptions' {toggleButtonsVisibility} -> toggleButtonsVisibility) (\s@PivotTableOptions' {} a -> s {toggleButtonsVisibility = a} :: PivotTableOptions)

instance Data.FromJSON PivotTableOptions where
  parseJSON =
    Data.withObject
      "PivotTableOptions"
      ( \x ->
          PivotTableOptions'
            Prelude.<$> (x Data..:? "CellStyle")
            Prelude.<*> (x Data..:? "ColumnHeaderStyle")
            Prelude.<*> (x Data..:? "ColumnNamesVisibility")
            Prelude.<*> (x Data..:? "MetricPlacement")
            Prelude.<*> (x Data..:? "RowAlternateColorOptions")
            Prelude.<*> (x Data..:? "RowFieldNamesStyle")
            Prelude.<*> (x Data..:? "RowHeaderStyle")
            Prelude.<*> (x Data..:? "SingleMetricVisibility")
            Prelude.<*> (x Data..:? "ToggleButtonsVisibility")
      )

instance Prelude.Hashable PivotTableOptions where
  hashWithSalt _salt PivotTableOptions' {..} =
    _salt `Prelude.hashWithSalt` cellStyle
      `Prelude.hashWithSalt` columnHeaderStyle
      `Prelude.hashWithSalt` columnNamesVisibility
      `Prelude.hashWithSalt` metricPlacement
      `Prelude.hashWithSalt` rowAlternateColorOptions
      `Prelude.hashWithSalt` rowFieldNamesStyle
      `Prelude.hashWithSalt` rowHeaderStyle
      `Prelude.hashWithSalt` singleMetricVisibility
      `Prelude.hashWithSalt` toggleButtonsVisibility

instance Prelude.NFData PivotTableOptions where
  rnf PivotTableOptions' {..} =
    Prelude.rnf cellStyle
      `Prelude.seq` Prelude.rnf columnHeaderStyle
      `Prelude.seq` Prelude.rnf columnNamesVisibility
      `Prelude.seq` Prelude.rnf metricPlacement
      `Prelude.seq` Prelude.rnf rowAlternateColorOptions
      `Prelude.seq` Prelude.rnf rowFieldNamesStyle
      `Prelude.seq` Prelude.rnf rowHeaderStyle
      `Prelude.seq` Prelude.rnf singleMetricVisibility
      `Prelude.seq` Prelude.rnf toggleButtonsVisibility

instance Data.ToJSON PivotTableOptions where
  toJSON PivotTableOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CellStyle" Data..=) Prelude.<$> cellStyle,
            ("ColumnHeaderStyle" Data..=)
              Prelude.<$> columnHeaderStyle,
            ("ColumnNamesVisibility" Data..=)
              Prelude.<$> columnNamesVisibility,
            ("MetricPlacement" Data..=)
              Prelude.<$> metricPlacement,
            ("RowAlternateColorOptions" Data..=)
              Prelude.<$> rowAlternateColorOptions,
            ("RowFieldNamesStyle" Data..=)
              Prelude.<$> rowFieldNamesStyle,
            ("RowHeaderStyle" Data..=)
              Prelude.<$> rowHeaderStyle,
            ("SingleMetricVisibility" Data..=)
              Prelude.<$> singleMetricVisibility,
            ("ToggleButtonsVisibility" Data..=)
              Prelude.<$> toggleButtonsVisibility
          ]
      )
