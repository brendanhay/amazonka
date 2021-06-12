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
-- Module      : Network.AWS.EC2.ModifyClientVpnEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Client VPN endpoint. Modifying the DNS server
-- resets existing client connections.
module Network.AWS.EC2.ModifyClientVpnEndpoint
  ( -- * Creating a Request
    ModifyClientVpnEndpoint (..),
    newModifyClientVpnEndpoint,

    -- * Request Lenses
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_vpnPort,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_clientVpnEndpointId,

    -- * Destructuring the Response
    ModifyClientVpnEndpointResponse (..),
    newModifyClientVpnEndpointResponse,

    -- * Response Lenses
    modifyClientVpnEndpointResponse_return,
    modifyClientVpnEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyClientVpnEndpoint' smart constructor.
data ModifyClientVpnEndpoint = ModifyClientVpnEndpoint'
  { -- | The IDs of one or more security groups to apply to the target network.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ARN of the server certificate to be used. The server certificate
    -- must be provisioned in AWS Certificate Manager (ACM).
    serverCertificateArn :: Core.Maybe Core.Text,
    -- | Information about the client connection logging options.
    --
    -- If you enable client connection logging, data about client connections
    -- is sent to a Cloudwatch Logs log stream. The following information is
    -- logged:
    --
    -- -   Client connection requests
    --
    -- -   Client connection results (successful and unsuccessful)
    --
    -- -   Reasons for unsuccessful client connection requests
    --
    -- -   Client connection termination time
    connectionLogOptions :: Core.Maybe ConnectionLogOptions,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Core.Maybe ClientConnectOptions,
    -- | Information about the DNS servers to be used by Client VPN connections.
    -- A Client VPN endpoint can have up to two DNS servers.
    dnsServers :: Core.Maybe DnsServersOptionsModifyStructure,
    -- | The port number to assign to the Client VPN endpoint for TCP and UDP
    -- traffic.
    --
    -- Valid Values: @443@ | @1194@
    --
    -- Default Value: @443@
    vpnPort :: Core.Maybe Core.Int,
    -- | A brief description of the Client VPN endpoint.
    description :: Core.Maybe Core.Text,
    -- | The ID of the VPC to associate with the Client VPN endpoint.
    vpcId :: Core.Maybe Core.Text,
    -- | Specify whether to enable the self-service portal for the Client VPN
    -- endpoint.
    selfServicePortal :: Core.Maybe SelfServicePortal,
    -- | Indicates whether the VPN is split-tunnel.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
    -- in the /AWS Client VPN Administrator Guide/.
    splitTunnel :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint to modify.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'modifyClientVpnEndpoint_securityGroupIds' - The IDs of one or more security groups to apply to the target network.
--
-- 'dryRun', 'modifyClientVpnEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serverCertificateArn', 'modifyClientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate to be used. The server certificate
-- must be provisioned in AWS Certificate Manager (ACM).
--
-- 'connectionLogOptions', 'modifyClientVpnEndpoint_connectionLogOptions' - Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections
-- is sent to a Cloudwatch Logs log stream. The following information is
-- logged:
--
-- -   Client connection requests
--
-- -   Client connection results (successful and unsuccessful)
--
-- -   Reasons for unsuccessful client connection requests
--
-- -   Client connection termination time
--
-- 'clientConnectOptions', 'modifyClientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'dnsServers', 'modifyClientVpnEndpoint_dnsServers' - Information about the DNS servers to be used by Client VPN connections.
-- A Client VPN endpoint can have up to two DNS servers.
--
-- 'vpnPort', 'modifyClientVpnEndpoint_vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
--
-- 'description', 'modifyClientVpnEndpoint_description' - A brief description of the Client VPN endpoint.
--
-- 'vpcId', 'modifyClientVpnEndpoint_vpcId' - The ID of the VPC to associate with the Client VPN endpoint.
--
-- 'selfServicePortal', 'modifyClientVpnEndpoint_selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- 'splitTunnel', 'modifyClientVpnEndpoint_splitTunnel' - Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
--
-- 'clientVpnEndpointId', 'modifyClientVpnEndpoint_clientVpnEndpointId' - The ID of the Client VPN endpoint to modify.
newModifyClientVpnEndpoint ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  ModifyClientVpnEndpoint
newModifyClientVpnEndpoint pClientVpnEndpointId_ =
  ModifyClientVpnEndpoint'
    { securityGroupIds =
        Core.Nothing,
      dryRun = Core.Nothing,
      serverCertificateArn = Core.Nothing,
      connectionLogOptions = Core.Nothing,
      clientConnectOptions = Core.Nothing,
      dnsServers = Core.Nothing,
      vpnPort = Core.Nothing,
      description = Core.Nothing,
      vpcId = Core.Nothing,
      selfServicePortal = Core.Nothing,
      splitTunnel = Core.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | The IDs of one or more security groups to apply to the target network.
modifyClientVpnEndpoint_securityGroupIds :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe [Core.Text])
modifyClientVpnEndpoint_securityGroupIds = Lens.lens (\ModifyClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@ModifyClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: ModifyClientVpnEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyClientVpnEndpoint_dryRun :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Bool)
modifyClientVpnEndpoint_dryRun = Lens.lens (\ModifyClientVpnEndpoint' {dryRun} -> dryRun) (\s@ModifyClientVpnEndpoint' {} a -> s {dryRun = a} :: ModifyClientVpnEndpoint)

-- | The ARN of the server certificate to be used. The server certificate
-- must be provisioned in AWS Certificate Manager (ACM).
modifyClientVpnEndpoint_serverCertificateArn :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Text)
modifyClientVpnEndpoint_serverCertificateArn = Lens.lens (\ModifyClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@ModifyClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: ModifyClientVpnEndpoint)

-- | Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections
-- is sent to a Cloudwatch Logs log stream. The following information is
-- logged:
--
-- -   Client connection requests
--
-- -   Client connection results (successful and unsuccessful)
--
-- -   Reasons for unsuccessful client connection requests
--
-- -   Client connection termination time
modifyClientVpnEndpoint_connectionLogOptions :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe ConnectionLogOptions)
modifyClientVpnEndpoint_connectionLogOptions = Lens.lens (\ModifyClientVpnEndpoint' {connectionLogOptions} -> connectionLogOptions) (\s@ModifyClientVpnEndpoint' {} a -> s {connectionLogOptions = a} :: ModifyClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
modifyClientVpnEndpoint_clientConnectOptions :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe ClientConnectOptions)
modifyClientVpnEndpoint_clientConnectOptions = Lens.lens (\ModifyClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@ModifyClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: ModifyClientVpnEndpoint)

-- | Information about the DNS servers to be used by Client VPN connections.
-- A Client VPN endpoint can have up to two DNS servers.
modifyClientVpnEndpoint_dnsServers :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe DnsServersOptionsModifyStructure)
modifyClientVpnEndpoint_dnsServers = Lens.lens (\ModifyClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@ModifyClientVpnEndpoint' {} a -> s {dnsServers = a} :: ModifyClientVpnEndpoint)

-- | The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
modifyClientVpnEndpoint_vpnPort :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Int)
modifyClientVpnEndpoint_vpnPort = Lens.lens (\ModifyClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@ModifyClientVpnEndpoint' {} a -> s {vpnPort = a} :: ModifyClientVpnEndpoint)

-- | A brief description of the Client VPN endpoint.
modifyClientVpnEndpoint_description :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Text)
modifyClientVpnEndpoint_description = Lens.lens (\ModifyClientVpnEndpoint' {description} -> description) (\s@ModifyClientVpnEndpoint' {} a -> s {description = a} :: ModifyClientVpnEndpoint)

-- | The ID of the VPC to associate with the Client VPN endpoint.
modifyClientVpnEndpoint_vpcId :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Text)
modifyClientVpnEndpoint_vpcId = Lens.lens (\ModifyClientVpnEndpoint' {vpcId} -> vpcId) (\s@ModifyClientVpnEndpoint' {} a -> s {vpcId = a} :: ModifyClientVpnEndpoint)

-- | Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
modifyClientVpnEndpoint_selfServicePortal :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe SelfServicePortal)
modifyClientVpnEndpoint_selfServicePortal = Lens.lens (\ModifyClientVpnEndpoint' {selfServicePortal} -> selfServicePortal) (\s@ModifyClientVpnEndpoint' {} a -> s {selfServicePortal = a} :: ModifyClientVpnEndpoint)

-- | Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
modifyClientVpnEndpoint_splitTunnel :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Bool)
modifyClientVpnEndpoint_splitTunnel = Lens.lens (\ModifyClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@ModifyClientVpnEndpoint' {} a -> s {splitTunnel = a} :: ModifyClientVpnEndpoint)

-- | The ID of the Client VPN endpoint to modify.
modifyClientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' ModifyClientVpnEndpoint Core.Text
modifyClientVpnEndpoint_clientVpnEndpointId = Lens.lens (\ModifyClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ModifyClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: ModifyClientVpnEndpoint)

instance Core.AWSRequest ModifyClientVpnEndpoint where
  type
    AWSResponse ModifyClientVpnEndpoint =
      ModifyClientVpnEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyClientVpnEndpointResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClientVpnEndpoint

instance Core.NFData ModifyClientVpnEndpoint

instance Core.ToHeaders ModifyClientVpnEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClientVpnEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClientVpnEndpoint where
  toQuery ModifyClientVpnEndpoint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClientVpnEndpoint" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Core.<$> securityGroupIds
          ),
        "DryRun" Core.=: dryRun,
        "ServerCertificateArn" Core.=: serverCertificateArn,
        "ConnectionLogOptions" Core.=: connectionLogOptions,
        "ClientConnectOptions" Core.=: clientConnectOptions,
        "DnsServers" Core.=: dnsServers,
        "VpnPort" Core.=: vpnPort,
        "Description" Core.=: description,
        "VpcId" Core.=: vpcId,
        "SelfServicePortal" Core.=: selfServicePortal,
        "SplitTunnel" Core.=: splitTunnel,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newModifyClientVpnEndpointResponse' smart constructor.
data ModifyClientVpnEndpointResponse = ModifyClientVpnEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClientVpnEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyClientVpnEndpointResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyClientVpnEndpointResponse_httpStatus' - The response's http status code.
newModifyClientVpnEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClientVpnEndpointResponse
newModifyClientVpnEndpointResponse pHttpStatus_ =
  ModifyClientVpnEndpointResponse'
    { return' =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyClientVpnEndpointResponse_return :: Lens.Lens' ModifyClientVpnEndpointResponse (Core.Maybe Core.Bool)
modifyClientVpnEndpointResponse_return = Lens.lens (\ModifyClientVpnEndpointResponse' {return'} -> return') (\s@ModifyClientVpnEndpointResponse' {} a -> s {return' = a} :: ModifyClientVpnEndpointResponse)

-- | The response's http status code.
modifyClientVpnEndpointResponse_httpStatus :: Lens.Lens' ModifyClientVpnEndpointResponse Core.Int
modifyClientVpnEndpointResponse_httpStatus = Lens.lens (\ModifyClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: ModifyClientVpnEndpointResponse)

instance Core.NFData ModifyClientVpnEndpointResponse
