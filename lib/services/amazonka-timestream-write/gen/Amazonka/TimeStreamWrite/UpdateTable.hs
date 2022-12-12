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
-- Module      : Amazonka.TimeStreamWrite.UpdateTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the retention duration of the memory store and magnetic store
-- for your Timestream table. Note that the change in retention duration
-- takes effect immediately. For example, if the retention period of the
-- memory store was initially set to 2 hours and then changed to 24 hours,
-- the memory store will be capable of holding 24 hours of data, but will
-- be populated with 24 hours of data 22 hours after this change was made.
-- Timestream does not retrieve data from the magnetic store to populate
-- the memory store.
--
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.update-table.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_magneticStoreWriteProperties,
    updateTable_retentionProperties,
    updateTable_databaseName,
    updateTable_tableName,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_table,
    updateTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | Contains properties to set on the table when enabling magnetic store
    -- writes.
    magneticStoreWriteProperties :: Prelude.Maybe MagneticStoreWriteProperties,
    -- | The retention duration of the memory store and the magnetic store.
    retentionProperties :: Prelude.Maybe RetentionProperties,
    -- | The name of the Timestream database.
    databaseName :: Prelude.Text,
    -- | The name of the Timestream table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'magneticStoreWriteProperties', 'updateTable_magneticStoreWriteProperties' - Contains properties to set on the table when enabling magnetic store
-- writes.
--
-- 'retentionProperties', 'updateTable_retentionProperties' - The retention duration of the memory store and the magnetic store.
--
-- 'databaseName', 'updateTable_databaseName' - The name of the Timestream database.
--
-- 'tableName', 'updateTable_tableName' - The name of the Timestream table.
newUpdateTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  UpdateTable
newUpdateTable pDatabaseName_ pTableName_ =
  UpdateTable'
    { magneticStoreWriteProperties =
        Prelude.Nothing,
      retentionProperties = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | Contains properties to set on the table when enabling magnetic store
-- writes.
updateTable_magneticStoreWriteProperties :: Lens.Lens' UpdateTable (Prelude.Maybe MagneticStoreWriteProperties)
updateTable_magneticStoreWriteProperties = Lens.lens (\UpdateTable' {magneticStoreWriteProperties} -> magneticStoreWriteProperties) (\s@UpdateTable' {} a -> s {magneticStoreWriteProperties = a} :: UpdateTable)

-- | The retention duration of the memory store and the magnetic store.
updateTable_retentionProperties :: Lens.Lens' UpdateTable (Prelude.Maybe RetentionProperties)
updateTable_retentionProperties = Lens.lens (\UpdateTable' {retentionProperties} -> retentionProperties) (\s@UpdateTable' {} a -> s {retentionProperties = a} :: UpdateTable)

-- | The name of the Timestream database.
updateTable_databaseName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_databaseName = Lens.lens (\UpdateTable' {databaseName} -> databaseName) (\s@UpdateTable' {} a -> s {databaseName = a} :: UpdateTable)

-- | The name of the Timestream table.
updateTable_tableName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_tableName = Lens.lens (\UpdateTable' {tableName} -> tableName) (\s@UpdateTable' {} a -> s {tableName = a} :: UpdateTable)

instance Core.AWSRequest UpdateTable where
  type AWSResponse UpdateTable = UpdateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Prelude.<$> (x Data..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTable where
  hashWithSalt _salt UpdateTable' {..} =
    _salt
      `Prelude.hashWithSalt` magneticStoreWriteProperties
      `Prelude.hashWithSalt` retentionProperties
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData UpdateTable where
  rnf UpdateTable' {..} =
    Prelude.rnf magneticStoreWriteProperties
      `Prelude.seq` Prelude.rnf retentionProperties
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders UpdateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.UpdateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MagneticStoreWriteProperties" Data..=)
              Prelude.<$> magneticStoreWriteProperties,
            ("RetentionProperties" Data..=)
              Prelude.<$> retentionProperties,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath UpdateTable where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | The updated Timestream table.
    table :: Prelude.Maybe Table,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'table', 'updateTableResponse_table' - The updated Timestream table.
--
-- 'httpStatus', 'updateTableResponse_httpStatus' - The response's http status code.
newUpdateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ =
  UpdateTableResponse'
    { table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated Timestream table.
updateTableResponse_table :: Lens.Lens' UpdateTableResponse (Prelude.Maybe Table)
updateTableResponse_table = Lens.lens (\UpdateTableResponse' {table} -> table) (\s@UpdateTableResponse' {} a -> s {table = a} :: UpdateTableResponse)

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Prelude.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

instance Prelude.NFData UpdateTableResponse where
  rnf UpdateTableResponse' {..} =
    Prelude.rnf table
      `Prelude.seq` Prelude.rnf httpStatus
