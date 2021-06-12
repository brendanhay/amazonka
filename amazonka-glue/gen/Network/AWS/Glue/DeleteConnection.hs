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
-- Module      : Network.AWS.Glue.DeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection from the Data Catalog.
module Network.AWS.Glue.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_catalogId,
    deleteConnection_connectionName,

    -- * Destructuring the Response
    DeleteConnectionResponse (..),
    newDeleteConnectionResponse,

    -- * Response Lenses
    deleteConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the connection to delete.
    connectionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
--
-- 'connectionName', 'deleteConnection_connectionName' - The name of the connection to delete.
newDeleteConnection ::
  -- | 'connectionName'
  Core.Text ->
  DeleteConnection
newDeleteConnection pConnectionName_ =
  DeleteConnection'
    { catalogId = Core.Nothing,
      connectionName = pConnectionName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
deleteConnection_catalogId :: Lens.Lens' DeleteConnection (Core.Maybe Core.Text)
deleteConnection_catalogId = Lens.lens (\DeleteConnection' {catalogId} -> catalogId) (\s@DeleteConnection' {} a -> s {catalogId = a} :: DeleteConnection)

-- | The name of the connection to delete.
deleteConnection_connectionName :: Lens.Lens' DeleteConnection Core.Text
deleteConnection_connectionName = Lens.lens (\DeleteConnection' {connectionName} -> connectionName) (\s@DeleteConnection' {} a -> s {connectionName = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteConnection

instance Core.NFData DeleteConnection

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("ConnectionName" Core..= connectionName)
          ]
      )

instance Core.ToPath DeleteConnection where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConnectionResponse_httpStatus' - The response's http status code.
newDeleteConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Core.NFData DeleteConnectionResponse
