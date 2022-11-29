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
-- Module      : Amazonka.Glue.DeleteConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection from the Data Catalog.
module Amazonka.Glue.DeleteConnection
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection to delete.
    connectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'connectionName', 'deleteConnection_connectionName' - The name of the connection to delete.
newDeleteConnection ::
  -- | 'connectionName'
  Prelude.Text ->
  DeleteConnection
newDeleteConnection pConnectionName_ =
  DeleteConnection'
    { catalogId = Prelude.Nothing,
      connectionName = pConnectionName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
deleteConnection_catalogId :: Lens.Lens' DeleteConnection (Prelude.Maybe Prelude.Text)
deleteConnection_catalogId = Lens.lens (\DeleteConnection' {catalogId} -> catalogId) (\s@DeleteConnection' {} a -> s {catalogId = a} :: DeleteConnection)

-- | The name of the connection to delete.
deleteConnection_connectionName :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_connectionName = Lens.lens (\DeleteConnection' {connectionName} -> connectionName) (\s@DeleteConnection' {} a -> s {connectionName = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnection where
  hashWithSalt _salt DeleteConnection' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf connectionName

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteConnection" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just
              ("ConnectionName" Core..= connectionName)
          ]
      )

instance Core.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Prelude.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Prelude.NFData DeleteConnectionResponse where
  rnf DeleteConnectionResponse' {..} =
    Prelude.rnf httpStatus
