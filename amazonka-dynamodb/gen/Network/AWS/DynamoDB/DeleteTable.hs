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
-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteTable@ operation deletes a table and all of its items. After
-- a @DeleteTable@ request, the specified table is in the @DELETING@ state
-- until DynamoDB completes the deletion. If the table is in the @ACTIVE@
-- state, you can delete it. If a table is in @CREATING@ or @UPDATING@
-- states, then DynamoDB returns a @ResourceInUseException@. If the
-- specified table does not exist, DynamoDB returns a
-- @ResourceNotFoundException@. If table is already in the @DELETING@
-- state, no error is returned.
--
-- DynamoDB might continue to accept data read and write operations, such
-- as @GetItem@ and @PutItem@, on a table in the @DELETING@ state until the
-- table deletion is complete.
--
-- When you delete a table, any indexes on that table are also deleted.
--
-- If you have DynamoDB Streams enabled on the table, then the
-- corresponding stream on that table goes into the @DISABLED@ state, and
-- the stream is automatically deleted after 24 hours.
--
-- Use the @DescribeTable@ action to check the status of the table.
module Network.AWS.DynamoDB.DeleteTable
  ( -- * Creating a Request
    DeleteTable (..),
    newDeleteTable,

    -- * Request Lenses
    deleteTable_tableName,

    -- * Destructuring the Response
    DeleteTableResponse (..),
    newDeleteTableResponse,

    -- * Response Lenses
    deleteTableResponse_tableDescription,
    deleteTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteTable@ operation.
--
-- /See:/ 'newDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { -- | The name of the table to delete.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'deleteTable_tableName' - The name of the table to delete.
newDeleteTable ::
  -- | 'tableName'
  Core.Text ->
  DeleteTable
newDeleteTable pTableName_ =
  DeleteTable' {tableName = pTableName_}

-- | The name of the table to delete.
deleteTable_tableName :: Lens.Lens' DeleteTable Core.Text
deleteTable_tableName = Lens.lens (\DeleteTable' {tableName} -> tableName) (\s@DeleteTable' {} a -> s {tableName = a} :: DeleteTable)

instance Core.AWSRequest DeleteTable where
  type AWSResponse DeleteTable = DeleteTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTableResponse'
            Core.<$> (x Core..?> "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTable

instance Core.NFData DeleteTable

instance Core.ToHeaders DeleteTable where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DynamoDB_20120810.DeleteTable" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TableName" Core..= tableName)]
      )

instance Core.ToPath DeleteTable where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTable where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteTable@ operation.
--
-- /See:/ 'newDeleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
  { -- | Represents the properties of a table.
    tableDescription :: Core.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableDescription', 'deleteTableResponse_tableDescription' - Represents the properties of a table.
--
-- 'httpStatus', 'deleteTableResponse_httpStatus' - The response's http status code.
newDeleteTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTableResponse
newDeleteTableResponse pHttpStatus_ =
  DeleteTableResponse'
    { tableDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of a table.
deleteTableResponse_tableDescription :: Lens.Lens' DeleteTableResponse (Core.Maybe TableDescription)
deleteTableResponse_tableDescription = Lens.lens (\DeleteTableResponse' {tableDescription} -> tableDescription) (\s@DeleteTableResponse' {} a -> s {tableDescription = a} :: DeleteTableResponse)

-- | The response's http status code.
deleteTableResponse_httpStatus :: Lens.Lens' DeleteTableResponse Core.Int
deleteTableResponse_httpStatus = Lens.lens (\DeleteTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTableResponse' {} a -> s {httpStatus = a} :: DeleteTableResponse)

instance Core.NFData DeleteTableResponse
