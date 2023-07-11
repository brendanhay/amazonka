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
-- Module      : Amazonka.HoneyCode.Types.UpdateRowData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.UpdateRowData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.CellInput
import qualified Amazonka.Prelude as Prelude

-- | Data needed to create a single row in a table as part of the
-- BatchCreateTableRows request.
--
-- /See:/ 'newUpdateRowData' smart constructor.
data UpdateRowData = UpdateRowData'
  { -- | The id of the row that needs to be updated.
    rowId :: Prelude.Text,
    -- | A map representing the cells to update in the given row. The key is the
    -- column id of the cell and the value is the CellInput object that
    -- represents the data to set in that cell.
    cellsToUpdate :: Prelude.HashMap Prelude.Text CellInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRowData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowId', 'updateRowData_rowId' - The id of the row that needs to be updated.
--
-- 'cellsToUpdate', 'updateRowData_cellsToUpdate' - A map representing the cells to update in the given row. The key is the
-- column id of the cell and the value is the CellInput object that
-- represents the data to set in that cell.
newUpdateRowData ::
  -- | 'rowId'
  Prelude.Text ->
  UpdateRowData
newUpdateRowData pRowId_ =
  UpdateRowData'
    { rowId = pRowId_,
      cellsToUpdate = Prelude.mempty
    }

-- | The id of the row that needs to be updated.
updateRowData_rowId :: Lens.Lens' UpdateRowData Prelude.Text
updateRowData_rowId = Lens.lens (\UpdateRowData' {rowId} -> rowId) (\s@UpdateRowData' {} a -> s {rowId = a} :: UpdateRowData)

-- | A map representing the cells to update in the given row. The key is the
-- column id of the cell and the value is the CellInput object that
-- represents the data to set in that cell.
updateRowData_cellsToUpdate :: Lens.Lens' UpdateRowData (Prelude.HashMap Prelude.Text CellInput)
updateRowData_cellsToUpdate = Lens.lens (\UpdateRowData' {cellsToUpdate} -> cellsToUpdate) (\s@UpdateRowData' {} a -> s {cellsToUpdate = a} :: UpdateRowData) Prelude.. Lens.coerced

instance Prelude.Hashable UpdateRowData where
  hashWithSalt _salt UpdateRowData' {..} =
    _salt
      `Prelude.hashWithSalt` rowId
      `Prelude.hashWithSalt` cellsToUpdate

instance Prelude.NFData UpdateRowData where
  rnf UpdateRowData' {..} =
    Prelude.rnf rowId
      `Prelude.seq` Prelude.rnf cellsToUpdate

instance Data.ToJSON UpdateRowData where
  toJSON UpdateRowData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("rowId" Data..= rowId),
            Prelude.Just
              ("cellsToUpdate" Data..= cellsToUpdate)
          ]
      )
