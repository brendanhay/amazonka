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
-- Module      : Amazonka.Glue.Types.MicrosoftSQLServerCatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MicrosoftSQLServerCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Microsoft SQL server data source in the Glue Data Catalog.
--
-- /See:/ 'newMicrosoftSQLServerCatalogSource' smart constructor.
data MicrosoftSQLServerCatalogSource = MicrosoftSQLServerCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MicrosoftSQLServerCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'microsoftSQLServerCatalogSource_name' - The name of the data source.
--
-- 'database', 'microsoftSQLServerCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'microsoftSQLServerCatalogSource_table' - The name of the table in the database to read from.
newMicrosoftSQLServerCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  MicrosoftSQLServerCatalogSource
newMicrosoftSQLServerCatalogSource
  pName_
  pDatabase_
  pTable_ =
    MicrosoftSQLServerCatalogSource'
      { name = pName_,
        database = pDatabase_,
        table = pTable_
      }

-- | The name of the data source.
microsoftSQLServerCatalogSource_name :: Lens.Lens' MicrosoftSQLServerCatalogSource Prelude.Text
microsoftSQLServerCatalogSource_name = Lens.lens (\MicrosoftSQLServerCatalogSource' {name} -> name) (\s@MicrosoftSQLServerCatalogSource' {} a -> s {name = a} :: MicrosoftSQLServerCatalogSource)

-- | The name of the database to read from.
microsoftSQLServerCatalogSource_database :: Lens.Lens' MicrosoftSQLServerCatalogSource Prelude.Text
microsoftSQLServerCatalogSource_database = Lens.lens (\MicrosoftSQLServerCatalogSource' {database} -> database) (\s@MicrosoftSQLServerCatalogSource' {} a -> s {database = a} :: MicrosoftSQLServerCatalogSource)

-- | The name of the table in the database to read from.
microsoftSQLServerCatalogSource_table :: Lens.Lens' MicrosoftSQLServerCatalogSource Prelude.Text
microsoftSQLServerCatalogSource_table = Lens.lens (\MicrosoftSQLServerCatalogSource' {table} -> table) (\s@MicrosoftSQLServerCatalogSource' {} a -> s {table = a} :: MicrosoftSQLServerCatalogSource)

instance
  Data.FromJSON
    MicrosoftSQLServerCatalogSource
  where
  parseJSON =
    Data.withObject
      "MicrosoftSQLServerCatalogSource"
      ( \x ->
          MicrosoftSQLServerCatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance
  Prelude.Hashable
    MicrosoftSQLServerCatalogSource
  where
  hashWithSalt
    _salt
    MicrosoftSQLServerCatalogSource' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` database
        `Prelude.hashWithSalt` table

instance
  Prelude.NFData
    MicrosoftSQLServerCatalogSource
  where
  rnf MicrosoftSQLServerCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON MicrosoftSQLServerCatalogSource where
  toJSON MicrosoftSQLServerCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
