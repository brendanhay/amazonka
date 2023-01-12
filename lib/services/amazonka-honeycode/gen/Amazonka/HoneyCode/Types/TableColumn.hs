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
-- Module      : Amazonka.HoneyCode.Types.TableColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.TableColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.Format
import qualified Amazonka.Prelude as Prelude

-- | An object that contains attributes about a single column in a table
--
-- /See:/ 'newTableColumn' smart constructor.
data TableColumn = TableColumn'
  { -- | The column level format that is applied in the table. An empty value in
    -- this field means that the column format is the default value \'AUTO\'.
    format :: Prelude.Maybe Format,
    -- | The id of the column in the table.
    tableColumnId :: Prelude.Maybe Prelude.Text,
    -- | The name of the column in the table.
    tableColumnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'tableColumn_format' - The column level format that is applied in the table. An empty value in
-- this field means that the column format is the default value \'AUTO\'.
--
-- 'tableColumnId', 'tableColumn_tableColumnId' - The id of the column in the table.
--
-- 'tableColumnName', 'tableColumn_tableColumnName' - The name of the column in the table.
newTableColumn ::
  TableColumn
newTableColumn =
  TableColumn'
    { format = Prelude.Nothing,
      tableColumnId = Prelude.Nothing,
      tableColumnName = Prelude.Nothing
    }

-- | The column level format that is applied in the table. An empty value in
-- this field means that the column format is the default value \'AUTO\'.
tableColumn_format :: Lens.Lens' TableColumn (Prelude.Maybe Format)
tableColumn_format = Lens.lens (\TableColumn' {format} -> format) (\s@TableColumn' {} a -> s {format = a} :: TableColumn)

-- | The id of the column in the table.
tableColumn_tableColumnId :: Lens.Lens' TableColumn (Prelude.Maybe Prelude.Text)
tableColumn_tableColumnId = Lens.lens (\TableColumn' {tableColumnId} -> tableColumnId) (\s@TableColumn' {} a -> s {tableColumnId = a} :: TableColumn)

-- | The name of the column in the table.
tableColumn_tableColumnName :: Lens.Lens' TableColumn (Prelude.Maybe Prelude.Text)
tableColumn_tableColumnName = Lens.lens (\TableColumn' {tableColumnName} -> tableColumnName) (\s@TableColumn' {} a -> s {tableColumnName = a} :: TableColumn)

instance Data.FromJSON TableColumn where
  parseJSON =
    Data.withObject
      "TableColumn"
      ( \x ->
          TableColumn'
            Prelude.<$> (x Data..:? "format")
            Prelude.<*> (x Data..:? "tableColumnId")
            Prelude.<*> (x Data..:? "tableColumnName")
      )

instance Prelude.Hashable TableColumn where
  hashWithSalt _salt TableColumn' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` tableColumnId
      `Prelude.hashWithSalt` tableColumnName

instance Prelude.NFData TableColumn where
  rnf TableColumn' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf tableColumnId
      `Prelude.seq` Prelude.rnf tableColumnName
