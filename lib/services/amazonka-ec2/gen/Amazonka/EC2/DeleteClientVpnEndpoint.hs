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
-- Module      : Amazonka.EC2.DeleteClientVpnEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Client VPN endpoint. You must disassociate all
-- target networks before you can delete a Client VPN endpoint.
module Amazonka.EC2.DeleteClientVpnEndpoint
  ( -- * Creating a Request
    DeleteClientVpnEndpoint (..),
    newDeleteClientVpnEndpoint,

    -- * Request Lenses
    deleteClientVpnEndpoint_dryRun,
    deleteClientVpnEndpoint_clientVpnEndpointId,

    -- * Destructuring the Response
    DeleteClientVpnEndpointResponse (..),
    newDeleteClientVpnEndpointResponse,

    -- * Response Lenses
    deleteClientVpnEndpointResponse_status,
    deleteClientVpnEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteClientVpnEndpoint' smart constructor.
data DeleteClientVpnEndpoint = DeleteClientVpnEndpoint'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Client VPN to be deleted.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteClientVpnEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientVpnEndpointId', 'deleteClientVpnEndpoint_clientVpnEndpointId' - The ID of the Client VPN to be deleted.
newDeleteClientVpnEndpoint ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  DeleteClientVpnEndpoint
newDeleteClientVpnEndpoint pClientVpnEndpointId_ =
  DeleteClientVpnEndpoint'
    { dryRun = Prelude.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteClientVpnEndpoint_dryRun :: Lens.Lens' DeleteClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
deleteClientVpnEndpoint_dryRun = Lens.lens (\DeleteClientVpnEndpoint' {dryRun} -> dryRun) (\s@DeleteClientVpnEndpoint' {} a -> s {dryRun = a} :: DeleteClientVpnEndpoint)

-- | The ID of the Client VPN to be deleted.
deleteClientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' DeleteClientVpnEndpoint Prelude.Text
deleteClientVpnEndpoint_clientVpnEndpointId = Lens.lens (\DeleteClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DeleteClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: DeleteClientVpnEndpoint)

instance Core.AWSRequest DeleteClientVpnEndpoint where
  type
    AWSResponse DeleteClientVpnEndpoint =
      DeleteClientVpnEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteClientVpnEndpointResponse'
            Prelude.<$> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClientVpnEndpoint where
  hashWithSalt _salt DeleteClientVpnEndpoint' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` clientVpnEndpointId

instance Prelude.NFData DeleteClientVpnEndpoint where
  rnf DeleteClientVpnEndpoint' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance Data.ToHeaders DeleteClientVpnEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteClientVpnEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteClientVpnEndpoint where
  toQuery DeleteClientVpnEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteClientVpnEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDeleteClientVpnEndpointResponse' smart constructor.
data DeleteClientVpnEndpointResponse = DeleteClientVpnEndpointResponse'
  { -- | The current state of the Client VPN endpoint.
    status :: Prelude.Maybe ClientVpnEndpointStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientVpnEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteClientVpnEndpointResponse_status' - The current state of the Client VPN endpoint.
--
-- 'httpStatus', 'deleteClientVpnEndpointResponse_httpStatus' - The response's http status code.
newDeleteClientVpnEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClientVpnEndpointResponse
newDeleteClientVpnEndpointResponse pHttpStatus_ =
  DeleteClientVpnEndpointResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the Client VPN endpoint.
deleteClientVpnEndpointResponse_status :: Lens.Lens' DeleteClientVpnEndpointResponse (Prelude.Maybe ClientVpnEndpointStatus)
deleteClientVpnEndpointResponse_status = Lens.lens (\DeleteClientVpnEndpointResponse' {status} -> status) (\s@DeleteClientVpnEndpointResponse' {} a -> s {status = a} :: DeleteClientVpnEndpointResponse)

-- | The response's http status code.
deleteClientVpnEndpointResponse_httpStatus :: Lens.Lens' DeleteClientVpnEndpointResponse Prelude.Int
deleteClientVpnEndpointResponse_httpStatus = Lens.lens (\DeleteClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: DeleteClientVpnEndpointResponse)

instance
  Prelude.NFData
    DeleteClientVpnEndpointResponse
  where
  rnf DeleteClientVpnEndpointResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
