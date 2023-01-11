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
-- Module      : Amazonka.MacieV2.Types.Cell
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Cell where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of an occurrence of sensitive data in a Microsoft
-- Excel workbook, CSV file, or TSV file.
--
-- /See:/ 'newCell' smart constructor.
data Cell = Cell'
  { -- | The location of the cell, as an absolute cell reference, that contains
    -- the sensitive data, for example Sheet2!C5 for cell C5 on Sheet2 in a
    -- Microsoft Excel workbook. This value is null for CSV and TSV files.
    cellReference :: Prelude.Maybe Prelude.Text,
    -- | The column number of the column that contains the sensitive data. For a
    -- Microsoft Excel workbook, this value correlates to the alphabetical
    -- character(s) for a column identifier, for example: 1 for column A, 2 for
    -- column B, and so on.
    column :: Prelude.Maybe Prelude.Integer,
    -- | The name of the column that contains the sensitive data, if available.
    columnName :: Prelude.Maybe Prelude.Text,
    -- | The row number of the row that contains the sensitive data.
    row :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellReference', 'cell_cellReference' - The location of the cell, as an absolute cell reference, that contains
-- the sensitive data, for example Sheet2!C5 for cell C5 on Sheet2 in a
-- Microsoft Excel workbook. This value is null for CSV and TSV files.
--
-- 'column', 'cell_column' - The column number of the column that contains the sensitive data. For a
-- Microsoft Excel workbook, this value correlates to the alphabetical
-- character(s) for a column identifier, for example: 1 for column A, 2 for
-- column B, and so on.
--
-- 'columnName', 'cell_columnName' - The name of the column that contains the sensitive data, if available.
--
-- 'row', 'cell_row' - The row number of the row that contains the sensitive data.
newCell ::
  Cell
newCell =
  Cell'
    { cellReference = Prelude.Nothing,
      column = Prelude.Nothing,
      columnName = Prelude.Nothing,
      row = Prelude.Nothing
    }

-- | The location of the cell, as an absolute cell reference, that contains
-- the sensitive data, for example Sheet2!C5 for cell C5 on Sheet2 in a
-- Microsoft Excel workbook. This value is null for CSV and TSV files.
cell_cellReference :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_cellReference = Lens.lens (\Cell' {cellReference} -> cellReference) (\s@Cell' {} a -> s {cellReference = a} :: Cell)

-- | The column number of the column that contains the sensitive data. For a
-- Microsoft Excel workbook, this value correlates to the alphabetical
-- character(s) for a column identifier, for example: 1 for column A, 2 for
-- column B, and so on.
cell_column :: Lens.Lens' Cell (Prelude.Maybe Prelude.Integer)
cell_column = Lens.lens (\Cell' {column} -> column) (\s@Cell' {} a -> s {column = a} :: Cell)

-- | The name of the column that contains the sensitive data, if available.
cell_columnName :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_columnName = Lens.lens (\Cell' {columnName} -> columnName) (\s@Cell' {} a -> s {columnName = a} :: Cell)

-- | The row number of the row that contains the sensitive data.
cell_row :: Lens.Lens' Cell (Prelude.Maybe Prelude.Integer)
cell_row = Lens.lens (\Cell' {row} -> row) (\s@Cell' {} a -> s {row = a} :: Cell)

instance Data.FromJSON Cell where
  parseJSON =
    Data.withObject
      "Cell"
      ( \x ->
          Cell'
            Prelude.<$> (x Data..:? "cellReference")
            Prelude.<*> (x Data..:? "column")
            Prelude.<*> (x Data..:? "columnName")
            Prelude.<*> (x Data..:? "row")
      )

instance Prelude.Hashable Cell where
  hashWithSalt _salt Cell' {..} =
    _salt `Prelude.hashWithSalt` cellReference
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` row

instance Prelude.NFData Cell where
  rnf Cell' {..} =
    Prelude.rnf cellReference
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf row
