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
-- Module      : Amazonka.IoTSiteWise.DeletePortal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a portal from IoT SiteWise Monitor.
module Amazonka.IoTSiteWise.DeletePortal
  ( -- * Creating a Request
    DeletePortal (..),
    newDeletePortal,

    -- * Request Lenses
    deletePortal_clientToken,
    deletePortal_portalId,

    -- * Destructuring the Response
    DeletePortalResponse (..),
    newDeletePortalResponse,

    -- * Response Lenses
    deletePortalResponse_httpStatus,
    deletePortalResponse_portalStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePortal' smart constructor.
data DeletePortal = DeletePortal'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the portal to delete.
    portalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePortal_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'portalId', 'deletePortal_portalId' - The ID of the portal to delete.
newDeletePortal ::
  -- | 'portalId'
  Prelude.Text ->
  DeletePortal
newDeletePortal pPortalId_ =
  DeletePortal'
    { clientToken = Prelude.Nothing,
      portalId = pPortalId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deletePortal_clientToken :: Lens.Lens' DeletePortal (Prelude.Maybe Prelude.Text)
deletePortal_clientToken = Lens.lens (\DeletePortal' {clientToken} -> clientToken) (\s@DeletePortal' {} a -> s {clientToken = a} :: DeletePortal)

-- | The ID of the portal to delete.
deletePortal_portalId :: Lens.Lens' DeletePortal Prelude.Text
deletePortal_portalId = Lens.lens (\DeletePortal' {portalId} -> portalId) (\s@DeletePortal' {} a -> s {portalId = a} :: DeletePortal)

instance Core.AWSRequest DeletePortal where
  type AWSResponse DeletePortal = DeletePortalResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePortalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalStatus")
      )

instance Prelude.Hashable DeletePortal where
  hashWithSalt _salt DeletePortal' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` portalId

instance Prelude.NFData DeletePortal where
  rnf DeletePortal' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf portalId

instance Data.ToHeaders DeletePortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePortal where
  toPath DeletePortal' {..} =
    Prelude.mconcat ["/portals/", Data.toBS portalId]

instance Data.ToQuery DeletePortal where
  toQuery DeletePortal' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeletePortalResponse' smart constructor.
data DeletePortalResponse = DeletePortalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the portal, which contains a state (@DELETING@ after
    -- successfully calling this operation) and any error message.
    portalStatus :: PortalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePortalResponse_httpStatus' - The response's http status code.
--
-- 'portalStatus', 'deletePortalResponse_portalStatus' - The status of the portal, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
newDeletePortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalStatus'
  PortalStatus ->
  DeletePortalResponse
newDeletePortalResponse pHttpStatus_ pPortalStatus_ =
  DeletePortalResponse'
    { httpStatus = pHttpStatus_,
      portalStatus = pPortalStatus_
    }

-- | The response's http status code.
deletePortalResponse_httpStatus :: Lens.Lens' DeletePortalResponse Prelude.Int
deletePortalResponse_httpStatus = Lens.lens (\DeletePortalResponse' {httpStatus} -> httpStatus) (\s@DeletePortalResponse' {} a -> s {httpStatus = a} :: DeletePortalResponse)

-- | The status of the portal, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
deletePortalResponse_portalStatus :: Lens.Lens' DeletePortalResponse PortalStatus
deletePortalResponse_portalStatus = Lens.lens (\DeletePortalResponse' {portalStatus} -> portalStatus) (\s@DeletePortalResponse' {} a -> s {portalStatus = a} :: DeletePortalResponse)

instance Prelude.NFData DeletePortalResponse where
  rnf DeletePortalResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalStatus
