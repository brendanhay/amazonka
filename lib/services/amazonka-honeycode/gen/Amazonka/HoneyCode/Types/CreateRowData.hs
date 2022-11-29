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
-- Module      : Amazonka.HoneyCode.Types.CreateRowData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.CreateRowData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HoneyCode.Types.CellInput
import qualified Amazonka.Prelude as Prelude

-- | Data needed to create a single row in a table as part of the
-- BatchCreateTableRows request.
--
-- /See:/ 'newCreateRowData' smart constructor.
data CreateRowData = CreateRowData'
  { -- | An external identifier that represents the single row that is being
    -- created as part of the BatchCreateTableRows request. This can be any
    -- string that you can use to identify the row in the request. The
    -- BatchCreateTableRows API puts the batch item id in the results to allow
    -- you to link data in the request to data in the results.
    batchItemId :: Prelude.Text,
    -- | A map representing the cells to create in the new row. The key is the
    -- column id of the cell and the value is the CellInput object that
    -- represents the data to set in that cell.
    cellsToCreate :: Prelude.HashMap Prelude.Text CellInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRowData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchItemId', 'createRowData_batchItemId' - An external identifier that represents the single row that is being
-- created as part of the BatchCreateTableRows request. This can be any
-- string that you can use to identify the row in the request. The
-- BatchCreateTableRows API puts the batch item id in the results to allow
-- you to link data in the request to data in the results.
--
-- 'cellsToCreate', 'createRowData_cellsToCreate' - A map representing the cells to create in the new row. The key is the
-- column id of the cell and the value is the CellInput object that
-- represents the data to set in that cell.
newCreateRowData ::
  -- | 'batchItemId'
  Prelude.Text ->
  CreateRowData
newCreateRowData pBatchItemId_ =
  CreateRowData'
    { batchItemId = pBatchItemId_,
      cellsToCreate = Prelude.mempty
    }

-- | An external identifier that represents the single row that is being
-- created as part of the BatchCreateTableRows request. This can be any
-- string that you can use to identify the row in the request. The
-- BatchCreateTableRows API puts the batch item id in the results to allow
-- you to link data in the request to data in the results.
createRowData_batchItemId :: Lens.Lens' CreateRowData Prelude.Text
createRowData_batchItemId = Lens.lens (\CreateRowData' {batchItemId} -> batchItemId) (\s@CreateRowData' {} a -> s {batchItemId = a} :: CreateRowData)

-- | A map representing the cells to create in the new row. The key is the
-- column id of the cell and the value is the CellInput object that
-- represents the data to set in that cell.
createRowData_cellsToCreate :: Lens.Lens' CreateRowData (Prelude.HashMap Prelude.Text CellInput)
createRowData_cellsToCreate = Lens.lens (\CreateRowData' {cellsToCreate} -> cellsToCreate) (\s@CreateRowData' {} a -> s {cellsToCreate = a} :: CreateRowData) Prelude.. Lens.coerced

instance Prelude.Hashable CreateRowData where
  hashWithSalt _salt CreateRowData' {..} =
    _salt `Prelude.hashWithSalt` batchItemId
      `Prelude.hashWithSalt` cellsToCreate

instance Prelude.NFData CreateRowData where
  rnf CreateRowData' {..} =
    Prelude.rnf batchItemId
      `Prelude.seq` Prelude.rnf cellsToCreate

instance Core.ToJSON CreateRowData where
  toJSON CreateRowData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("batchItemId" Core..= batchItemId),
            Prelude.Just
              ("cellsToCreate" Core..= cellsToCreate)
          ]
      )
