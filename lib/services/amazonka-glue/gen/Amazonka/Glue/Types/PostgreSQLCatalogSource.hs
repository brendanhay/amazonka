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
-- Module      : Amazonka.Glue.Types.PostgreSQLCatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PostgreSQLCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a PostgresSQL data source in the Glue Data Catalog.
--
-- /See:/ 'newPostgreSQLCatalogSource' smart constructor.
data PostgreSQLCatalogSource = PostgreSQLCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostgreSQLCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'postgreSQLCatalogSource_name' - The name of the data source.
--
-- 'database', 'postgreSQLCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'postgreSQLCatalogSource_table' - The name of the table in the database to read from.
newPostgreSQLCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  PostgreSQLCatalogSource
newPostgreSQLCatalogSource pName_ pDatabase_ pTable_ =
  PostgreSQLCatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data source.
postgreSQLCatalogSource_name :: Lens.Lens' PostgreSQLCatalogSource Prelude.Text
postgreSQLCatalogSource_name = Lens.lens (\PostgreSQLCatalogSource' {name} -> name) (\s@PostgreSQLCatalogSource' {} a -> s {name = a} :: PostgreSQLCatalogSource)

-- | The name of the database to read from.
postgreSQLCatalogSource_database :: Lens.Lens' PostgreSQLCatalogSource Prelude.Text
postgreSQLCatalogSource_database = Lens.lens (\PostgreSQLCatalogSource' {database} -> database) (\s@PostgreSQLCatalogSource' {} a -> s {database = a} :: PostgreSQLCatalogSource)

-- | The name of the table in the database to read from.
postgreSQLCatalogSource_table :: Lens.Lens' PostgreSQLCatalogSource Prelude.Text
postgreSQLCatalogSource_table = Lens.lens (\PostgreSQLCatalogSource' {table} -> table) (\s@PostgreSQLCatalogSource' {} a -> s {table = a} :: PostgreSQLCatalogSource)

instance Data.FromJSON PostgreSQLCatalogSource where
  parseJSON =
    Data.withObject
      "PostgreSQLCatalogSource"
      ( \x ->
          PostgreSQLCatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable PostgreSQLCatalogSource where
  hashWithSalt _salt PostgreSQLCatalogSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData PostgreSQLCatalogSource where
  rnf PostgreSQLCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON PostgreSQLCatalogSource where
  toJSON PostgreSQLCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
