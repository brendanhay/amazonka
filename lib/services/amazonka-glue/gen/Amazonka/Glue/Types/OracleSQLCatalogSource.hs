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
-- Module      : Amazonka.Glue.Types.OracleSQLCatalogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.OracleSQLCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Oracle data source in the Glue Data Catalog.
--
-- /See:/ 'newOracleSQLCatalogSource' smart constructor.
data OracleSQLCatalogSource = OracleSQLCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OracleSQLCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'oracleSQLCatalogSource_name' - The name of the data source.
--
-- 'database', 'oracleSQLCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'oracleSQLCatalogSource_table' - The name of the table in the database to read from.
newOracleSQLCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  OracleSQLCatalogSource
newOracleSQLCatalogSource pName_ pDatabase_ pTable_ =
  OracleSQLCatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data source.
oracleSQLCatalogSource_name :: Lens.Lens' OracleSQLCatalogSource Prelude.Text
oracleSQLCatalogSource_name = Lens.lens (\OracleSQLCatalogSource' {name} -> name) (\s@OracleSQLCatalogSource' {} a -> s {name = a} :: OracleSQLCatalogSource)

-- | The name of the database to read from.
oracleSQLCatalogSource_database :: Lens.Lens' OracleSQLCatalogSource Prelude.Text
oracleSQLCatalogSource_database = Lens.lens (\OracleSQLCatalogSource' {database} -> database) (\s@OracleSQLCatalogSource' {} a -> s {database = a} :: OracleSQLCatalogSource)

-- | The name of the table in the database to read from.
oracleSQLCatalogSource_table :: Lens.Lens' OracleSQLCatalogSource Prelude.Text
oracleSQLCatalogSource_table = Lens.lens (\OracleSQLCatalogSource' {table} -> table) (\s@OracleSQLCatalogSource' {} a -> s {table = a} :: OracleSQLCatalogSource)

instance Data.FromJSON OracleSQLCatalogSource where
  parseJSON =
    Data.withObject
      "OracleSQLCatalogSource"
      ( \x ->
          OracleSQLCatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable OracleSQLCatalogSource where
  hashWithSalt _salt OracleSQLCatalogSource' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData OracleSQLCatalogSource where
  rnf OracleSQLCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON OracleSQLCatalogSource where
  toJSON OracleSQLCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
