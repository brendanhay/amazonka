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
-- Module      : Amazonka.QuickSight.Types.TableOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RowAlternateColorOptions
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.TableOrientation

-- | The table options for a table visual.
--
-- /See:/ 'newTableOptions' smart constructor.
data TableOptions = TableOptions'
  { -- | The table cell style of table cells.
    cellStyle :: Prelude.Maybe TableCellStyle,
    -- | The table cell style of a table header.
    headerStyle :: Prelude.Maybe TableCellStyle,
    -- | The orientation (vertical, horizontal) for a table.
    orientation :: Prelude.Maybe TableOrientation,
    -- | The row alternate color options (widget status, row alternate colors)
    -- for a table.
    rowAlternateColorOptions :: Prelude.Maybe RowAlternateColorOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellStyle', 'tableOptions_cellStyle' - The table cell style of table cells.
--
-- 'headerStyle', 'tableOptions_headerStyle' - The table cell style of a table header.
--
-- 'orientation', 'tableOptions_orientation' - The orientation (vertical, horizontal) for a table.
--
-- 'rowAlternateColorOptions', 'tableOptions_rowAlternateColorOptions' - The row alternate color options (widget status, row alternate colors)
-- for a table.
newTableOptions ::
  TableOptions
newTableOptions =
  TableOptions'
    { cellStyle = Prelude.Nothing,
      headerStyle = Prelude.Nothing,
      orientation = Prelude.Nothing,
      rowAlternateColorOptions = Prelude.Nothing
    }

-- | The table cell style of table cells.
tableOptions_cellStyle :: Lens.Lens' TableOptions (Prelude.Maybe TableCellStyle)
tableOptions_cellStyle = Lens.lens (\TableOptions' {cellStyle} -> cellStyle) (\s@TableOptions' {} a -> s {cellStyle = a} :: TableOptions)

-- | The table cell style of a table header.
tableOptions_headerStyle :: Lens.Lens' TableOptions (Prelude.Maybe TableCellStyle)
tableOptions_headerStyle = Lens.lens (\TableOptions' {headerStyle} -> headerStyle) (\s@TableOptions' {} a -> s {headerStyle = a} :: TableOptions)

-- | The orientation (vertical, horizontal) for a table.
tableOptions_orientation :: Lens.Lens' TableOptions (Prelude.Maybe TableOrientation)
tableOptions_orientation = Lens.lens (\TableOptions' {orientation} -> orientation) (\s@TableOptions' {} a -> s {orientation = a} :: TableOptions)

-- | The row alternate color options (widget status, row alternate colors)
-- for a table.
tableOptions_rowAlternateColorOptions :: Lens.Lens' TableOptions (Prelude.Maybe RowAlternateColorOptions)
tableOptions_rowAlternateColorOptions = Lens.lens (\TableOptions' {rowAlternateColorOptions} -> rowAlternateColorOptions) (\s@TableOptions' {} a -> s {rowAlternateColorOptions = a} :: TableOptions)

instance Data.FromJSON TableOptions where
  parseJSON =
    Data.withObject
      "TableOptions"
      ( \x ->
          TableOptions'
            Prelude.<$> (x Data..:? "CellStyle")
            Prelude.<*> (x Data..:? "HeaderStyle")
            Prelude.<*> (x Data..:? "Orientation")
            Prelude.<*> (x Data..:? "RowAlternateColorOptions")
      )

instance Prelude.Hashable TableOptions where
  hashWithSalt _salt TableOptions' {..} =
    _salt
      `Prelude.hashWithSalt` cellStyle
      `Prelude.hashWithSalt` headerStyle
      `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` rowAlternateColorOptions

instance Prelude.NFData TableOptions where
  rnf TableOptions' {..} =
    Prelude.rnf cellStyle
      `Prelude.seq` Prelude.rnf headerStyle
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf rowAlternateColorOptions

instance Data.ToJSON TableOptions where
  toJSON TableOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CellStyle" Data..=) Prelude.<$> cellStyle,
            ("HeaderStyle" Data..=) Prelude.<$> headerStyle,
            ("Orientation" Data..=) Prelude.<$> orientation,
            ("RowAlternateColorOptions" Data..=)
              Prelude.<$> rowAlternateColorOptions
          ]
      )
