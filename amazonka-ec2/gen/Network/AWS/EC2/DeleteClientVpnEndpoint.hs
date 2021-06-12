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
-- Module      : Network.AWS.EC2.DeleteClientVpnEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Client VPN endpoint. You must disassociate all
-- target networks before you can delete a Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVpnEndpoint
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteClientVpnEndpoint' smart constructor.
data DeleteClientVpnEndpoint = DeleteClientVpnEndpoint'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN to be deleted.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteClientVpnEndpoint
newDeleteClientVpnEndpoint pClientVpnEndpointId_ =
  DeleteClientVpnEndpoint'
    { dryRun = Core.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteClientVpnEndpoint_dryRun :: Lens.Lens' DeleteClientVpnEndpoint (Core.Maybe Core.Bool)
deleteClientVpnEndpoint_dryRun = Lens.lens (\DeleteClientVpnEndpoint' {dryRun} -> dryRun) (\s@DeleteClientVpnEndpoint' {} a -> s {dryRun = a} :: DeleteClientVpnEndpoint)

-- | The ID of the Client VPN to be deleted.
deleteClientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' DeleteClientVpnEndpoint Core.Text
deleteClientVpnEndpoint_clientVpnEndpointId = Lens.lens (\DeleteClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DeleteClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: DeleteClientVpnEndpoint)

instance Core.AWSRequest DeleteClientVpnEndpoint where
  type
    AWSResponse DeleteClientVpnEndpoint =
      DeleteClientVpnEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteClientVpnEndpointResponse'
            Core.<$> (x Core..@? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteClientVpnEndpoint

instance Core.NFData DeleteClientVpnEndpoint

instance Core.ToHeaders DeleteClientVpnEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteClientVpnEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DeleteClientVpnEndpoint where
  toQuery DeleteClientVpnEndpoint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteClientVpnEndpoint" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDeleteClientVpnEndpointResponse' smart constructor.
data DeleteClientVpnEndpointResponse = DeleteClientVpnEndpointResponse'
  { -- | The current state of the Client VPN endpoint.
    status :: Core.Maybe ClientVpnEndpointStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteClientVpnEndpointResponse
newDeleteClientVpnEndpointResponse pHttpStatus_ =
  DeleteClientVpnEndpointResponse'
    { status =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the Client VPN endpoint.
deleteClientVpnEndpointResponse_status :: Lens.Lens' DeleteClientVpnEndpointResponse (Core.Maybe ClientVpnEndpointStatus)
deleteClientVpnEndpointResponse_status = Lens.lens (\DeleteClientVpnEndpointResponse' {status} -> status) (\s@DeleteClientVpnEndpointResponse' {} a -> s {status = a} :: DeleteClientVpnEndpointResponse)

-- | The response's http status code.
deleteClientVpnEndpointResponse_httpStatus :: Lens.Lens' DeleteClientVpnEndpointResponse Core.Int
deleteClientVpnEndpointResponse_httpStatus = Lens.lens (\DeleteClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: DeleteClientVpnEndpointResponse)

instance Core.NFData DeleteClientVpnEndpointResponse
