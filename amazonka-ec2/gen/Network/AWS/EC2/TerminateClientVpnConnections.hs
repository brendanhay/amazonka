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
-- Module      : Network.AWS.EC2.TerminateClientVpnConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates active Client VPN endpoint connections. This action can be
-- used to terminate a specific client connection, or up to five
-- connections established by a specific user.
module Network.AWS.EC2.TerminateClientVpnConnections
  ( -- * Creating a Request
    TerminateClientVpnConnections (..),
    newTerminateClientVpnConnections,

    -- * Request Lenses
    terminateClientVpnConnections_dryRun,
    terminateClientVpnConnections_connectionId,
    terminateClientVpnConnections_username,
    terminateClientVpnConnections_clientVpnEndpointId,

    -- * Destructuring the Response
    TerminateClientVpnConnectionsResponse (..),
    newTerminateClientVpnConnectionsResponse,

    -- * Response Lenses
    terminateClientVpnConnectionsResponse_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_connectionStatuses,
    terminateClientVpnConnectionsResponse_username,
    terminateClientVpnConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTerminateClientVpnConnections' smart constructor.
data TerminateClientVpnConnections = TerminateClientVpnConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the client connection to be terminated.
    connectionId :: Core.Maybe Core.Text,
    -- | The name of the user who initiated the connection. Use this option to
    -- terminate all active connections for the specified user. This option can
    -- only be used if the user has established up to five connections.
    username :: Core.Maybe Core.Text,
    -- | The ID of the Client VPN endpoint to which the client is connected.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateClientVpnConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'terminateClientVpnConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionId', 'terminateClientVpnConnections_connectionId' - The ID of the client connection to be terminated.
--
-- 'username', 'terminateClientVpnConnections_username' - The name of the user who initiated the connection. Use this option to
-- terminate all active connections for the specified user. This option can
-- only be used if the user has established up to five connections.
--
-- 'clientVpnEndpointId', 'terminateClientVpnConnections_clientVpnEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
newTerminateClientVpnConnections ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  TerminateClientVpnConnections
newTerminateClientVpnConnections
  pClientVpnEndpointId_ =
    TerminateClientVpnConnections'
      { dryRun =
          Core.Nothing,
        connectionId = Core.Nothing,
        username = Core.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
terminateClientVpnConnections_dryRun :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Core.Bool)
terminateClientVpnConnections_dryRun = Lens.lens (\TerminateClientVpnConnections' {dryRun} -> dryRun) (\s@TerminateClientVpnConnections' {} a -> s {dryRun = a} :: TerminateClientVpnConnections)

-- | The ID of the client connection to be terminated.
terminateClientVpnConnections_connectionId :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Core.Text)
terminateClientVpnConnections_connectionId = Lens.lens (\TerminateClientVpnConnections' {connectionId} -> connectionId) (\s@TerminateClientVpnConnections' {} a -> s {connectionId = a} :: TerminateClientVpnConnections)

-- | The name of the user who initiated the connection. Use this option to
-- terminate all active connections for the specified user. This option can
-- only be used if the user has established up to five connections.
terminateClientVpnConnections_username :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Core.Text)
terminateClientVpnConnections_username = Lens.lens (\TerminateClientVpnConnections' {username} -> username) (\s@TerminateClientVpnConnections' {} a -> s {username = a} :: TerminateClientVpnConnections)

-- | The ID of the Client VPN endpoint to which the client is connected.
terminateClientVpnConnections_clientVpnEndpointId :: Lens.Lens' TerminateClientVpnConnections Core.Text
terminateClientVpnConnections_clientVpnEndpointId = Lens.lens (\TerminateClientVpnConnections' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@TerminateClientVpnConnections' {} a -> s {clientVpnEndpointId = a} :: TerminateClientVpnConnections)

instance
  Core.AWSRequest
    TerminateClientVpnConnections
  where
  type
    AWSResponse TerminateClientVpnConnections =
      TerminateClientVpnConnectionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          TerminateClientVpnConnectionsResponse'
            Core.<$> (x Core..@? "clientVpnEndpointId")
            Core.<*> ( x Core..@? "connectionStatuses" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "username")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TerminateClientVpnConnections

instance Core.NFData TerminateClientVpnConnections

instance Core.ToHeaders TerminateClientVpnConnections where
  toHeaders = Core.const Core.mempty

instance Core.ToPath TerminateClientVpnConnections where
  toPath = Core.const "/"

instance Core.ToQuery TerminateClientVpnConnections where
  toQuery TerminateClientVpnConnections' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("TerminateClientVpnConnections" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ConnectionId" Core.=: connectionId,
        "Username" Core.=: username,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newTerminateClientVpnConnectionsResponse' smart constructor.
data TerminateClientVpnConnectionsResponse = TerminateClientVpnConnectionsResponse'
  { -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the client connections.
    connectionStatuses :: Core.Maybe [TerminateConnectionStatus],
    -- | The user who established the terminated client connections.
    username :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateClientVpnConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'terminateClientVpnConnectionsResponse_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'connectionStatuses', 'terminateClientVpnConnectionsResponse_connectionStatuses' - The current state of the client connections.
--
-- 'username', 'terminateClientVpnConnectionsResponse_username' - The user who established the terminated client connections.
--
-- 'httpStatus', 'terminateClientVpnConnectionsResponse_httpStatus' - The response's http status code.
newTerminateClientVpnConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TerminateClientVpnConnectionsResponse
newTerminateClientVpnConnectionsResponse pHttpStatus_ =
  TerminateClientVpnConnectionsResponse'
    { clientVpnEndpointId =
        Core.Nothing,
      connectionStatuses = Core.Nothing,
      username = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Client VPN endpoint.
terminateClientVpnConnectionsResponse_clientVpnEndpointId :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe Core.Text)
terminateClientVpnConnectionsResponse_clientVpnEndpointId = Lens.lens (\TerminateClientVpnConnectionsResponse' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@TerminateClientVpnConnectionsResponse' {} a -> s {clientVpnEndpointId = a} :: TerminateClientVpnConnectionsResponse)

-- | The current state of the client connections.
terminateClientVpnConnectionsResponse_connectionStatuses :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe [TerminateConnectionStatus])
terminateClientVpnConnectionsResponse_connectionStatuses = Lens.lens (\TerminateClientVpnConnectionsResponse' {connectionStatuses} -> connectionStatuses) (\s@TerminateClientVpnConnectionsResponse' {} a -> s {connectionStatuses = a} :: TerminateClientVpnConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The user who established the terminated client connections.
terminateClientVpnConnectionsResponse_username :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe Core.Text)
terminateClientVpnConnectionsResponse_username = Lens.lens (\TerminateClientVpnConnectionsResponse' {username} -> username) (\s@TerminateClientVpnConnectionsResponse' {} a -> s {username = a} :: TerminateClientVpnConnectionsResponse)

-- | The response's http status code.
terminateClientVpnConnectionsResponse_httpStatus :: Lens.Lens' TerminateClientVpnConnectionsResponse Core.Int
terminateClientVpnConnectionsResponse_httpStatus = Lens.lens (\TerminateClientVpnConnectionsResponse' {httpStatus} -> httpStatus) (\s@TerminateClientVpnConnectionsResponse' {} a -> s {httpStatus = a} :: TerminateClientVpnConnectionsResponse)

instance
  Core.NFData
    TerminateClientVpnConnectionsResponse
