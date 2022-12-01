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
-- Module      : Amazonka.HoneyCode.Types.TableRow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.TableRow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HoneyCode.Types.Cell
import qualified Amazonka.Prelude as Prelude

-- | An object that contains attributes about a single row in a table
--
-- /See:/ 'newTableRow' smart constructor.
data TableRow = TableRow'
  { -- | The id of the row in the table.
    rowId :: Prelude.Text,
    -- | A list of cells in the table row. The cells appear in the same order as
    -- the columns of the table.
    cells :: [Core.Sensitive Cell]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableRow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowId', 'tableRow_rowId' - The id of the row in the table.
--
-- 'cells', 'tableRow_cells' - A list of cells in the table row. The cells appear in the same order as
-- the columns of the table.
newTableRow ::
  -- | 'rowId'
  Prelude.Text ->
  TableRow
newTableRow pRowId_ =
  TableRow' {rowId = pRowId_, cells = Prelude.mempty}

-- | The id of the row in the table.
tableRow_rowId :: Lens.Lens' TableRow Prelude.Text
tableRow_rowId = Lens.lens (\TableRow' {rowId} -> rowId) (\s@TableRow' {} a -> s {rowId = a} :: TableRow)

-- | A list of cells in the table row. The cells appear in the same order as
-- the columns of the table.
tableRow_cells :: Lens.Lens' TableRow [Cell]
tableRow_cells = Lens.lens (\TableRow' {cells} -> cells) (\s@TableRow' {} a -> s {cells = a} :: TableRow) Prelude.. Lens.coerced

instance Core.FromJSON TableRow where
  parseJSON =
    Core.withObject
      "TableRow"
      ( \x ->
          TableRow'
            Prelude.<$> (x Core..: "rowId")
            Prelude.<*> (x Core..:? "cells" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TableRow where
  hashWithSalt _salt TableRow' {..} =
    _salt `Prelude.hashWithSalt` rowId
      `Prelude.hashWithSalt` cells

instance Prelude.NFData TableRow where
  rnf TableRow' {..} =
    Prelude.rnf rowId `Prelude.seq` Prelude.rnf cells
