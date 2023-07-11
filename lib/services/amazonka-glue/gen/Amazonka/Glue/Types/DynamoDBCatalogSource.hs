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
-- Module      : Amazonka.Glue.Types.DynamoDBCatalogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DynamoDBCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a DynamoDB data source in the Glue Data Catalog.
--
-- /See:/ 'newDynamoDBCatalogSource' smart constructor.
data DynamoDBCatalogSource = DynamoDBCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamoDBCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dynamoDBCatalogSource_name' - The name of the data source.
--
-- 'database', 'dynamoDBCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'dynamoDBCatalogSource_table' - The name of the table in the database to read from.
newDynamoDBCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  DynamoDBCatalogSource
newDynamoDBCatalogSource pName_ pDatabase_ pTable_ =
  DynamoDBCatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data source.
dynamoDBCatalogSource_name :: Lens.Lens' DynamoDBCatalogSource Prelude.Text
dynamoDBCatalogSource_name = Lens.lens (\DynamoDBCatalogSource' {name} -> name) (\s@DynamoDBCatalogSource' {} a -> s {name = a} :: DynamoDBCatalogSource)

-- | The name of the database to read from.
dynamoDBCatalogSource_database :: Lens.Lens' DynamoDBCatalogSource Prelude.Text
dynamoDBCatalogSource_database = Lens.lens (\DynamoDBCatalogSource' {database} -> database) (\s@DynamoDBCatalogSource' {} a -> s {database = a} :: DynamoDBCatalogSource)

-- | The name of the table in the database to read from.
dynamoDBCatalogSource_table :: Lens.Lens' DynamoDBCatalogSource Prelude.Text
dynamoDBCatalogSource_table = Lens.lens (\DynamoDBCatalogSource' {table} -> table) (\s@DynamoDBCatalogSource' {} a -> s {table = a} :: DynamoDBCatalogSource)

instance Data.FromJSON DynamoDBCatalogSource where
  parseJSON =
    Data.withObject
      "DynamoDBCatalogSource"
      ( \x ->
          DynamoDBCatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable DynamoDBCatalogSource where
  hashWithSalt _salt DynamoDBCatalogSource' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData DynamoDBCatalogSource where
  rnf DynamoDBCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON DynamoDBCatalogSource where
  toJSON DynamoDBCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
