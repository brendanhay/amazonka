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
-- Module      : Network.AWS.Glue.UpdateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connection definition in the Data Catalog.
module Network.AWS.Glue.UpdateConnection
  ( -- * Creating a Request
    UpdateConnection (..),
    newUpdateConnection,

    -- * Request Lenses
    updateConnection_catalogId,
    updateConnection_name,
    updateConnection_connectionInput,

    -- * Destructuring the Response
    UpdateConnectionResponse (..),
    newUpdateConnectionResponse,

    -- * Response Lenses
    updateConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the connection definition to update.
    name :: Core.Text,
    -- | A @ConnectionInput@ object that redefines the connection in question.
    connectionInput :: ConnectionInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
--
-- 'name', 'updateConnection_name' - The name of the connection definition to update.
--
-- 'connectionInput', 'updateConnection_connectionInput' - A @ConnectionInput@ object that redefines the connection in question.
newUpdateConnection ::
  -- | 'name'
  Core.Text ->
  -- | 'connectionInput'
  ConnectionInput ->
  UpdateConnection
newUpdateConnection pName_ pConnectionInput_ =
  UpdateConnection'
    { catalogId = Core.Nothing,
      name = pName_,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
updateConnection_catalogId :: Lens.Lens' UpdateConnection (Core.Maybe Core.Text)
updateConnection_catalogId = Lens.lens (\UpdateConnection' {catalogId} -> catalogId) (\s@UpdateConnection' {} a -> s {catalogId = a} :: UpdateConnection)

-- | The name of the connection definition to update.
updateConnection_name :: Lens.Lens' UpdateConnection Core.Text
updateConnection_name = Lens.lens (\UpdateConnection' {name} -> name) (\s@UpdateConnection' {} a -> s {name = a} :: UpdateConnection)

-- | A @ConnectionInput@ object that redefines the connection in question.
updateConnection_connectionInput :: Lens.Lens' UpdateConnection ConnectionInput
updateConnection_connectionInput = Lens.lens (\UpdateConnection' {connectionInput} -> connectionInput) (\s@UpdateConnection' {} a -> s {connectionInput = a} :: UpdateConnection)

instance Core.AWSRequest UpdateConnection where
  type
    AWSResponse UpdateConnection =
      UpdateConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConnection

instance Core.NFData UpdateConnection

instance Core.ToHeaders UpdateConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("ConnectionInput" Core..= connectionInput)
          ]
      )

instance Core.ToPath UpdateConnection where
  toPath = Core.const "/"

instance Core.ToQuery UpdateConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateConnectionResponse' smart constructor.
data UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectionResponse_httpStatus' - The response's http status code.
newUpdateConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConnectionResponse
newUpdateConnectionResponse pHttpStatus_ =
  UpdateConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConnectionResponse_httpStatus :: Lens.Lens' UpdateConnectionResponse Core.Int
updateConnectionResponse_httpStatus = Lens.lens (\UpdateConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionResponse)

instance Core.NFData UpdateConnectionResponse
