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
-- Module      : Amazonka.HoneyCode.Types.ResultRow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ResultRow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.DataItem
import qualified Amazonka.Prelude as Prelude

-- | A single row in the ResultSet.
--
-- /See:/ 'newResultRow' smart constructor.
data ResultRow = ResultRow'
  { -- | The ID for a particular row.
    rowId :: Prelude.Maybe Prelude.Text,
    -- | List of all the data cells in a row.
    dataItems :: [Data.Sensitive DataItem]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultRow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowId', 'resultRow_rowId' - The ID for a particular row.
--
-- 'dataItems', 'resultRow_dataItems' - List of all the data cells in a row.
newResultRow ::
  ResultRow
newResultRow =
  ResultRow'
    { rowId = Prelude.Nothing,
      dataItems = Prelude.mempty
    }

-- | The ID for a particular row.
resultRow_rowId :: Lens.Lens' ResultRow (Prelude.Maybe Prelude.Text)
resultRow_rowId = Lens.lens (\ResultRow' {rowId} -> rowId) (\s@ResultRow' {} a -> s {rowId = a} :: ResultRow)

-- | List of all the data cells in a row.
resultRow_dataItems :: Lens.Lens' ResultRow [DataItem]
resultRow_dataItems = Lens.lens (\ResultRow' {dataItems} -> dataItems) (\s@ResultRow' {} a -> s {dataItems = a} :: ResultRow) Prelude.. Lens.coerced

instance Data.FromJSON ResultRow where
  parseJSON =
    Data.withObject
      "ResultRow"
      ( \x ->
          ResultRow'
            Prelude.<$> (x Data..:? "rowId")
            Prelude.<*> (x Data..:? "dataItems" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultRow where
  hashWithSalt _salt ResultRow' {..} =
    _salt
      `Prelude.hashWithSalt` rowId
      `Prelude.hashWithSalt` dataItems

instance Prelude.NFData ResultRow where
  rnf ResultRow' {..} =
    Prelude.rnf rowId
      `Prelude.seq` Prelude.rnf dataItems
