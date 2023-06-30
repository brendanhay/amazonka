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
-- Module      : Amazonka.TimeStreamQuery.Types.SelectColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.SelectColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.Type

-- | Details of the column that is returned by the query.
--
-- /See:/ 'newSelectColumn' smart constructor.
data SelectColumn = SelectColumn'
  { -- | True, if the column name was aliased by the query. False otherwise.
    aliased :: Prelude.Maybe Prelude.Bool,
    -- | Database that has this column.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Name of the column.
    name :: Prelude.Maybe Prelude.Text,
    -- | Table within the database that has this column.
    tableName :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliased', 'selectColumn_aliased' - True, if the column name was aliased by the query. False otherwise.
--
-- 'databaseName', 'selectColumn_databaseName' - Database that has this column.
--
-- 'name', 'selectColumn_name' - Name of the column.
--
-- 'tableName', 'selectColumn_tableName' - Table within the database that has this column.
--
-- 'type'', 'selectColumn_type' - Undocumented member.
newSelectColumn ::
  SelectColumn
newSelectColumn =
  SelectColumn'
    { aliased = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      name = Prelude.Nothing,
      tableName = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | True, if the column name was aliased by the query. False otherwise.
selectColumn_aliased :: Lens.Lens' SelectColumn (Prelude.Maybe Prelude.Bool)
selectColumn_aliased = Lens.lens (\SelectColumn' {aliased} -> aliased) (\s@SelectColumn' {} a -> s {aliased = a} :: SelectColumn)

-- | Database that has this column.
selectColumn_databaseName :: Lens.Lens' SelectColumn (Prelude.Maybe Prelude.Text)
selectColumn_databaseName = Lens.lens (\SelectColumn' {databaseName} -> databaseName) (\s@SelectColumn' {} a -> s {databaseName = a} :: SelectColumn)

-- | Name of the column.
selectColumn_name :: Lens.Lens' SelectColumn (Prelude.Maybe Prelude.Text)
selectColumn_name = Lens.lens (\SelectColumn' {name} -> name) (\s@SelectColumn' {} a -> s {name = a} :: SelectColumn)

-- | Table within the database that has this column.
selectColumn_tableName :: Lens.Lens' SelectColumn (Prelude.Maybe Prelude.Text)
selectColumn_tableName = Lens.lens (\SelectColumn' {tableName} -> tableName) (\s@SelectColumn' {} a -> s {tableName = a} :: SelectColumn)

-- | Undocumented member.
selectColumn_type :: Lens.Lens' SelectColumn (Prelude.Maybe Type)
selectColumn_type = Lens.lens (\SelectColumn' {type'} -> type') (\s@SelectColumn' {} a -> s {type' = a} :: SelectColumn)

instance Data.FromJSON SelectColumn where
  parseJSON =
    Data.withObject
      "SelectColumn"
      ( \x ->
          SelectColumn'
            Prelude.<$> (x Data..:? "Aliased")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SelectColumn where
  hashWithSalt _salt SelectColumn' {..} =
    _salt
      `Prelude.hashWithSalt` aliased
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SelectColumn where
  rnf SelectColumn' {..} =
    Prelude.rnf aliased
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf type'
