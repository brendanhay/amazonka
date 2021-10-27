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
-- Module      : Network.AWS.HoneyCode.Types.TableColumn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Types.TableColumn where

import qualified Network.AWS.Core as Core
import Network.AWS.HoneyCode.Types.Format
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains attributes about a single column in a table
--
-- /See:/ 'newTableColumn' smart constructor.
data TableColumn = TableColumn'
  { -- | The column level format that is applied in the table. An empty value in
    -- this field means that the column format is the default value \'AUTO\'.
    format :: Prelude.Maybe Format,
    -- | The name of the column in the table.
    tableColumnName :: Prelude.Maybe Prelude.Text,
    -- | The id of the column in the table.
    tableColumnId :: Prelude.Maybe Prelude.Text
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
-- 'tableColumnName', 'tableColumn_tableColumnName' - The name of the column in the table.
--
-- 'tableColumnId', 'tableColumn_tableColumnId' - The id of the column in the table.
newTableColumn ::
  TableColumn
newTableColumn =
  TableColumn'
    { format = Prelude.Nothing,
      tableColumnName = Prelude.Nothing,
      tableColumnId = Prelude.Nothing
    }

-- | The column level format that is applied in the table. An empty value in
-- this field means that the column format is the default value \'AUTO\'.
tableColumn_format :: Lens.Lens' TableColumn (Prelude.Maybe Format)
tableColumn_format = Lens.lens (\TableColumn' {format} -> format) (\s@TableColumn' {} a -> s {format = a} :: TableColumn)

-- | The name of the column in the table.
tableColumn_tableColumnName :: Lens.Lens' TableColumn (Prelude.Maybe Prelude.Text)
tableColumn_tableColumnName = Lens.lens (\TableColumn' {tableColumnName} -> tableColumnName) (\s@TableColumn' {} a -> s {tableColumnName = a} :: TableColumn)

-- | The id of the column in the table.
tableColumn_tableColumnId :: Lens.Lens' TableColumn (Prelude.Maybe Prelude.Text)
tableColumn_tableColumnId = Lens.lens (\TableColumn' {tableColumnId} -> tableColumnId) (\s@TableColumn' {} a -> s {tableColumnId = a} :: TableColumn)

instance Core.FromJSON TableColumn where
  parseJSON =
    Core.withObject
      "TableColumn"
      ( \x ->
          TableColumn'
            Prelude.<$> (x Core..:? "format")
            Prelude.<*> (x Core..:? "tableColumnName")
            Prelude.<*> (x Core..:? "tableColumnId")
      )

instance Prelude.Hashable TableColumn

instance Prelude.NFData TableColumn
