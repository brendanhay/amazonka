{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamWrite.CreateTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreateTable operation adds a new table to an existing database in
-- your account. In an Amazon Web Services account, table names must be at
-- least unique within each Region if they are in the same database. You
-- may have identical table names in the same Region if the tables are in
-- separate databases. While creating the table, you must specify the table
-- name, database name, and the retention properties.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.create-table.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.CreateTable
  ( -- * Creating a Request
    CreateTable (..),
    newCreateTable,

    -- * Request Lenses
    createTable_tags,
    createTable_retentionProperties,
    createTable_magneticStoreWriteProperties,
    createTable_databaseName,
    createTable_tableName,

    -- * Destructuring the Response
    CreateTableResponse (..),
    newCreateTableResponse,

    -- * Response Lenses
    createTableResponse_table,
    createTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | A list of key-value pairs to label the table.
    tags :: Prelude.Maybe [Tag],
    -- | The duration for which your time series data must be stored in the
    -- memory store and the magnetic store.
    retentionProperties :: Prelude.Maybe RetentionProperties,
    -- | Contains properties to set on the table when enabling magnetic store
    -- writes.
    magneticStoreWriteProperties :: Prelude.Maybe MagneticStoreWriteProperties,
    -- | The name of the Timestream database.
    databaseName :: Prelude.Text,
    -- | The name of the Timestream table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTable_tags' - A list of key-value pairs to label the table.
--
-- 'retentionProperties', 'createTable_retentionProperties' - The duration for which your time series data must be stored in the
-- memory store and the magnetic store.
--
-- 'magneticStoreWriteProperties', 'createTable_magneticStoreWriteProperties' - Contains properties to set on the table when enabling magnetic store
-- writes.
--
-- 'databaseName', 'createTable_databaseName' - The name of the Timestream database.
--
-- 'tableName', 'createTable_tableName' - The name of the Timestream table.
newCreateTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  CreateTable
newCreateTable pDatabaseName_ pTableName_ =
  CreateTable'
    { tags = Prelude.Nothing,
      retentionProperties = Prelude.Nothing,
      magneticStoreWriteProperties = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A list of key-value pairs to label the table.
createTable_tags :: Lens.Lens' CreateTable (Prelude.Maybe [Tag])
createTable_tags = Lens.lens (\CreateTable' {tags} -> tags) (\s@CreateTable' {} a -> s {tags = a} :: CreateTable) Prelude.. Lens.mapping Lens.coerced

-- | The duration for which your time series data must be stored in the
-- memory store and the magnetic store.
createTable_retentionProperties :: Lens.Lens' CreateTable (Prelude.Maybe RetentionProperties)
createTable_retentionProperties = Lens.lens (\CreateTable' {retentionProperties} -> retentionProperties) (\s@CreateTable' {} a -> s {retentionProperties = a} :: CreateTable)

-- | Contains properties to set on the table when enabling magnetic store
-- writes.
createTable_magneticStoreWriteProperties :: Lens.Lens' CreateTable (Prelude.Maybe MagneticStoreWriteProperties)
createTable_magneticStoreWriteProperties = Lens.lens (\CreateTable' {magneticStoreWriteProperties} -> magneticStoreWriteProperties) (\s@CreateTable' {} a -> s {magneticStoreWriteProperties = a} :: CreateTable)

-- | The name of the Timestream database.
createTable_databaseName :: Lens.Lens' CreateTable Prelude.Text
createTable_databaseName = Lens.lens (\CreateTable' {databaseName} -> databaseName) (\s@CreateTable' {} a -> s {databaseName = a} :: CreateTable)

-- | The name of the Timestream table.
createTable_tableName :: Lens.Lens' CreateTable Prelude.Text
createTable_tableName = Lens.lens (\CreateTable' {tableName} -> tableName) (\s@CreateTable' {} a -> s {tableName = a} :: CreateTable)

instance Core.AWSRequest CreateTable where
  type AWSResponse CreateTable = CreateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTableResponse'
            Prelude.<$> (x Data..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTable where
  hashWithSalt _salt CreateTable' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` retentionProperties
      `Prelude.hashWithSalt` magneticStoreWriteProperties
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData CreateTable where
  rnf CreateTable' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf retentionProperties
      `Prelude.seq` Prelude.rnf magneticStoreWriteProperties
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders CreateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.CreateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("RetentionProperties" Data..=)
              Prelude.<$> retentionProperties,
            ("MagneticStoreWriteProperties" Data..=)
              Prelude.<$> magneticStoreWriteProperties,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath CreateTable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | The newly created Timestream table.
    table :: Prelude.Maybe Table,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'table', 'createTableResponse_table' - The newly created Timestream table.
--
-- 'httpStatus', 'createTableResponse_httpStatus' - The response's http status code.
newCreateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTableResponse
newCreateTableResponse pHttpStatus_ =
  CreateTableResponse'
    { table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created Timestream table.
createTableResponse_table :: Lens.Lens' CreateTableResponse (Prelude.Maybe Table)
createTableResponse_table = Lens.lens (\CreateTableResponse' {table} -> table) (\s@CreateTableResponse' {} a -> s {table = a} :: CreateTableResponse)

-- | The response's http status code.
createTableResponse_httpStatus :: Lens.Lens' CreateTableResponse Prelude.Int
createTableResponse_httpStatus = Lens.lens (\CreateTableResponse' {httpStatus} -> httpStatus) (\s@CreateTableResponse' {} a -> s {httpStatus = a} :: CreateTableResponse)

instance Prelude.NFData CreateTableResponse where
  rnf CreateTableResponse' {..} =
    Prelude.rnf table
      `Prelude.seq` Prelude.rnf httpStatus
