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
-- Module      : Amazonka.TimeStreamWrite.DeleteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given Timestream table. This is an irreversible operation.
-- After a Timestream database table is deleted, the time series data
-- stored in the table cannot be recovered.
--
-- Due to the nature of distributed retries, the operation can return
-- either success or a ResourceNotFoundException. Clients should consider
-- them equivalent.
--
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.delete-table.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.DeleteTable
  ( -- * Creating a Request
    DeleteTable (..),
    newDeleteTable,

    -- * Request Lenses
    deleteTable_databaseName,
    deleteTable_tableName,

    -- * Destructuring the Response
    DeleteTableResponse (..),
    newDeleteTableResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { -- | The name of the database where the Timestream database is to be deleted.
    databaseName :: Prelude.Text,
    -- | The name of the Timestream table to be deleted.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'deleteTable_databaseName' - The name of the database where the Timestream database is to be deleted.
--
-- 'tableName', 'deleteTable_tableName' - The name of the Timestream table to be deleted.
newDeleteTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DeleteTable
newDeleteTable pDatabaseName_ pTableName_ =
  DeleteTable'
    { databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The name of the database where the Timestream database is to be deleted.
deleteTable_databaseName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_databaseName = Lens.lens (\DeleteTable' {databaseName} -> databaseName) (\s@DeleteTable' {} a -> s {databaseName = a} :: DeleteTable)

-- | The name of the Timestream table to be deleted.
deleteTable_tableName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_tableName = Lens.lens (\DeleteTable' {tableName} -> tableName) (\s@DeleteTable' {} a -> s {tableName = a} :: DeleteTable)

instance Core.AWSRequest DeleteTable where
  type AWSResponse DeleteTable = DeleteTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteTableResponse'

instance Prelude.Hashable DeleteTable where
  hashWithSalt _salt DeleteTable' {..} =
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DeleteTable where
  rnf DeleteTable' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders DeleteTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DeleteTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath DeleteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTableResponse ::
  DeleteTableResponse
newDeleteTableResponse = DeleteTableResponse'

instance Prelude.NFData DeleteTableResponse where
  rnf _ = ()
