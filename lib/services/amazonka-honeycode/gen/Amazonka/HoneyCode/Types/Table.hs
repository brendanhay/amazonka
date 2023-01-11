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
-- Module      : Amazonka.HoneyCode.Types.Table
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.Table where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the properties of a table in a workbook.
--
-- /See:/ 'newTable' smart constructor.
data Table = Table'
  { -- | The id of the table.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Table' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableId', 'table_tableId' - The id of the table.
--
-- 'tableName', 'table_tableName' - The name of the table.
newTable ::
  Table
newTable =
  Table'
    { tableId = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The id of the table.
table_tableId :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_tableId = Lens.lens (\Table' {tableId} -> tableId) (\s@Table' {} a -> s {tableId = a} :: Table)

-- | The name of the table.
table_tableName :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_tableName = Lens.lens (\Table' {tableName} -> tableName) (\s@Table' {} a -> s {tableName = a} :: Table)

instance Data.FromJSON Table where
  parseJSON =
    Data.withObject
      "Table"
      ( \x ->
          Table'
            Prelude.<$> (x Data..:? "tableId")
            Prelude.<*> (x Data..:? "tableName")
      )

instance Prelude.Hashable Table where
  hashWithSalt _salt Table' {..} =
    _salt `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Table where
  rnf Table' {..} =
    Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf tableName
