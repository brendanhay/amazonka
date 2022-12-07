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
-- Module      : Amazonka.Glue.Types.MySQLCatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MySQLCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a MySQL data source in the Glue Data Catalog.
--
-- /See:/ 'newMySQLCatalogSource' smart constructor.
data MySQLCatalogSource = MySQLCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MySQLCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'mySQLCatalogSource_name' - The name of the data source.
--
-- 'database', 'mySQLCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'mySQLCatalogSource_table' - The name of the table in the database to read from.
newMySQLCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  MySQLCatalogSource
newMySQLCatalogSource pName_ pDatabase_ pTable_ =
  MySQLCatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data source.
mySQLCatalogSource_name :: Lens.Lens' MySQLCatalogSource Prelude.Text
mySQLCatalogSource_name = Lens.lens (\MySQLCatalogSource' {name} -> name) (\s@MySQLCatalogSource' {} a -> s {name = a} :: MySQLCatalogSource)

-- | The name of the database to read from.
mySQLCatalogSource_database :: Lens.Lens' MySQLCatalogSource Prelude.Text
mySQLCatalogSource_database = Lens.lens (\MySQLCatalogSource' {database} -> database) (\s@MySQLCatalogSource' {} a -> s {database = a} :: MySQLCatalogSource)

-- | The name of the table in the database to read from.
mySQLCatalogSource_table :: Lens.Lens' MySQLCatalogSource Prelude.Text
mySQLCatalogSource_table = Lens.lens (\MySQLCatalogSource' {table} -> table) (\s@MySQLCatalogSource' {} a -> s {table = a} :: MySQLCatalogSource)

instance Data.FromJSON MySQLCatalogSource where
  parseJSON =
    Data.withObject
      "MySQLCatalogSource"
      ( \x ->
          MySQLCatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable MySQLCatalogSource where
  hashWithSalt _salt MySQLCatalogSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData MySQLCatalogSource where
  rnf MySQLCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON MySQLCatalogSource where
  toJSON MySQLCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
