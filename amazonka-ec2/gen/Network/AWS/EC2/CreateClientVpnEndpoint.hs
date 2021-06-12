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
-- Module      : Network.AWS.EC2.CreateClientVpnEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Client VPN endpoint. A Client VPN endpoint is the resource you
-- create and configure to enable and manage client VPN sessions. It is the
-- destination endpoint at which all client VPN sessions are terminated.
module Network.AWS.EC2.CreateClientVpnEndpoint
  ( -- * Creating a Request
    CreateClientVpnEndpoint (..),
    newCreateClientVpnEndpoint,

    -- * Request Lenses
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,

    -- * Destructuring the Response
    CreateClientVpnEndpointResponse (..),
    newCreateClientVpnEndpointResponse,

    -- * Response Lenses
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_status,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateClientVpnEndpoint' smart constructor.
data CreateClientVpnEndpoint = CreateClientVpnEndpoint'
  { -- | The IDs of one or more security groups to apply to the target network.
    -- You must also specify the ID of the VPC that contains the security
    -- groups.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The tags to apply to the Client VPN endpoint during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The transport protocol to be used by the VPN session.
    --
    -- Default value: @udp@
    transportProtocol :: Core.Maybe TransportProtocol,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Core.Maybe ClientConnectOptions,
    -- | Information about the DNS servers to be used for DNS resolution. A
    -- Client VPN endpoint can have up to two DNS servers. If no DNS server is
    -- specified, the DNS address configured on the device is used for the DNS
    -- server.
    dnsServers :: Core.Maybe [Core.Text],
    -- | The port number to assign to the Client VPN endpoint for TCP and UDP
    -- traffic.
    --
    -- Valid Values: @443@ | @1194@
    --
    -- Default Value: @443@
    vpnPort :: Core.Maybe Core.Int,
    -- | A brief description of the Client VPN endpoint.
    description :: Core.Maybe Core.Text,
    -- | The ID of the VPC to associate with the Client VPN endpoint. If no
    -- security group IDs are specified in the request, the default security
    -- group for the VPC is applied.
    vpcId :: Core.Maybe Core.Text,
    -- | Specify whether to enable the self-service portal for the Client VPN
    -- endpoint.
    --
    -- Default Value: @enabled@
    selfServicePortal :: Core.Maybe SelfServicePortal,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | Indicates whether split-tunnel is enabled on the AWS Client VPN
    -- endpoint.
    --
    -- By default, split-tunnel on a VPN endpoint is disabled.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
    -- in the /AWS Client VPN Administrator Guide/.
    splitTunnel :: Core.Maybe Core.Bool,
    -- | The IPv4 address range, in CIDR notation, from which to assign client IP
    -- addresses. The address range cannot overlap with the local CIDR of the
    -- VPC in which the associated subnet is located, or the routes that you
    -- add manually. The address range cannot be changed after the Client VPN
    -- endpoint has been created. The CIDR block should be \/22 or greater.
    clientCidrBlock :: Core.Text,
    -- | The ARN of the server certificate. For more information, see the
    -- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
    serverCertificateArn :: Core.Text,
    -- | Information about the authentication method to be used to authenticate
    -- clients.
    authenticationOptions :: [ClientVpnAuthenticationRequest],
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
    connectionLogOptions :: ConnectionLogOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createClientVpnEndpoint_securityGroupIds' - The IDs of one or more security groups to apply to the target network.
-- You must also specify the ID of the VPC that contains the security
-- groups.
--
-- 'tagSpecifications', 'createClientVpnEndpoint_tagSpecifications' - The tags to apply to the Client VPN endpoint during creation.
--
-- 'dryRun', 'createClientVpnEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transportProtocol', 'createClientVpnEndpoint_transportProtocol' - The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
--
-- 'clientConnectOptions', 'createClientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'dnsServers', 'createClientVpnEndpoint_dnsServers' - Information about the DNS servers to be used for DNS resolution. A
-- Client VPN endpoint can have up to two DNS servers. If no DNS server is
-- specified, the DNS address configured on the device is used for the DNS
-- server.
--
-- 'vpnPort', 'createClientVpnEndpoint_vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
--
-- 'description', 'createClientVpnEndpoint_description' - A brief description of the Client VPN endpoint.
--
-- 'vpcId', 'createClientVpnEndpoint_vpcId' - The ID of the VPC to associate with the Client VPN endpoint. If no
-- security group IDs are specified in the request, the default security
-- group for the VPC is applied.
--
-- 'selfServicePortal', 'createClientVpnEndpoint_selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- Default Value: @enabled@
--
-- 'clientToken', 'createClientVpnEndpoint_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'splitTunnel', 'createClientVpnEndpoint_splitTunnel' - Indicates whether split-tunnel is enabled on the AWS Client VPN
-- endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
--
-- 'clientCidrBlock', 'createClientVpnEndpoint_clientCidrBlock' - The IPv4 address range, in CIDR notation, from which to assign client IP
-- addresses. The address range cannot overlap with the local CIDR of the
-- VPC in which the associated subnet is located, or the routes that you
-- add manually. The address range cannot be changed after the Client VPN
-- endpoint has been created. The CIDR block should be \/22 or greater.
--
-- 'serverCertificateArn', 'createClientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
--
-- 'authenticationOptions', 'createClientVpnEndpoint_authenticationOptions' - Information about the authentication method to be used to authenticate
-- clients.
--
-- 'connectionLogOptions', 'createClientVpnEndpoint_connectionLogOptions' - Information about the client connection logging options.
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
newCreateClientVpnEndpoint ::
  -- | 'clientCidrBlock'
  Core.Text ->
  -- | 'serverCertificateArn'
  Core.Text ->
  -- | 'connectionLogOptions'
  ConnectionLogOptions ->
  CreateClientVpnEndpoint
newCreateClientVpnEndpoint
  pClientCidrBlock_
  pServerCertificateArn_
  pConnectionLogOptions_ =
    CreateClientVpnEndpoint'
      { securityGroupIds =
          Core.Nothing,
        tagSpecifications = Core.Nothing,
        dryRun = Core.Nothing,
        transportProtocol = Core.Nothing,
        clientConnectOptions = Core.Nothing,
        dnsServers = Core.Nothing,
        vpnPort = Core.Nothing,
        description = Core.Nothing,
        vpcId = Core.Nothing,
        selfServicePortal = Core.Nothing,
        clientToken = Core.Nothing,
        splitTunnel = Core.Nothing,
        clientCidrBlock = pClientCidrBlock_,
        serverCertificateArn = pServerCertificateArn_,
        authenticationOptions = Core.mempty,
        connectionLogOptions = pConnectionLogOptions_
      }

-- | The IDs of one or more security groups to apply to the target network.
-- You must also specify the ID of the VPC that contains the security
-- groups.
createClientVpnEndpoint_securityGroupIds :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [Core.Text])
createClientVpnEndpoint_securityGroupIds = Lens.lens (\CreateClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: CreateClientVpnEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The tags to apply to the Client VPN endpoint during creation.
createClientVpnEndpoint_tagSpecifications :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [TagSpecification])
createClientVpnEndpoint_tagSpecifications = Lens.lens (\CreateClientVpnEndpoint' {tagSpecifications} -> tagSpecifications) (\s@CreateClientVpnEndpoint' {} a -> s {tagSpecifications = a} :: CreateClientVpnEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createClientVpnEndpoint_dryRun :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Bool)
createClientVpnEndpoint_dryRun = Lens.lens (\CreateClientVpnEndpoint' {dryRun} -> dryRun) (\s@CreateClientVpnEndpoint' {} a -> s {dryRun = a} :: CreateClientVpnEndpoint)

-- | The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
createClientVpnEndpoint_transportProtocol :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe TransportProtocol)
createClientVpnEndpoint_transportProtocol = Lens.lens (\CreateClientVpnEndpoint' {transportProtocol} -> transportProtocol) (\s@CreateClientVpnEndpoint' {} a -> s {transportProtocol = a} :: CreateClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
createClientVpnEndpoint_clientConnectOptions :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe ClientConnectOptions)
createClientVpnEndpoint_clientConnectOptions = Lens.lens (\CreateClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@CreateClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: CreateClientVpnEndpoint)

-- | Information about the DNS servers to be used for DNS resolution. A
-- Client VPN endpoint can have up to two DNS servers. If no DNS server is
-- specified, the DNS address configured on the device is used for the DNS
-- server.
createClientVpnEndpoint_dnsServers :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [Core.Text])
createClientVpnEndpoint_dnsServers = Lens.lens (\CreateClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@CreateClientVpnEndpoint' {} a -> s {dnsServers = a} :: CreateClientVpnEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
createClientVpnEndpoint_vpnPort :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Int)
createClientVpnEndpoint_vpnPort = Lens.lens (\CreateClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@CreateClientVpnEndpoint' {} a -> s {vpnPort = a} :: CreateClientVpnEndpoint)

-- | A brief description of the Client VPN endpoint.
createClientVpnEndpoint_description :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Text)
createClientVpnEndpoint_description = Lens.lens (\CreateClientVpnEndpoint' {description} -> description) (\s@CreateClientVpnEndpoint' {} a -> s {description = a} :: CreateClientVpnEndpoint)

-- | The ID of the VPC to associate with the Client VPN endpoint. If no
-- security group IDs are specified in the request, the default security
-- group for the VPC is applied.
createClientVpnEndpoint_vpcId :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Text)
createClientVpnEndpoint_vpcId = Lens.lens (\CreateClientVpnEndpoint' {vpcId} -> vpcId) (\s@CreateClientVpnEndpoint' {} a -> s {vpcId = a} :: CreateClientVpnEndpoint)

-- | Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- Default Value: @enabled@
createClientVpnEndpoint_selfServicePortal :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe SelfServicePortal)
createClientVpnEndpoint_selfServicePortal = Lens.lens (\CreateClientVpnEndpoint' {selfServicePortal} -> selfServicePortal) (\s@CreateClientVpnEndpoint' {} a -> s {selfServicePortal = a} :: CreateClientVpnEndpoint)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createClientVpnEndpoint_clientToken :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Text)
createClientVpnEndpoint_clientToken = Lens.lens (\CreateClientVpnEndpoint' {clientToken} -> clientToken) (\s@CreateClientVpnEndpoint' {} a -> s {clientToken = a} :: CreateClientVpnEndpoint)

-- | Indicates whether split-tunnel is enabled on the AWS Client VPN
-- endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
createClientVpnEndpoint_splitTunnel :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Bool)
createClientVpnEndpoint_splitTunnel = Lens.lens (\CreateClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@CreateClientVpnEndpoint' {} a -> s {splitTunnel = a} :: CreateClientVpnEndpoint)

-- | The IPv4 address range, in CIDR notation, from which to assign client IP
-- addresses. The address range cannot overlap with the local CIDR of the
-- VPC in which the associated subnet is located, or the routes that you
-- add manually. The address range cannot be changed after the Client VPN
-- endpoint has been created. The CIDR block should be \/22 or greater.
createClientVpnEndpoint_clientCidrBlock :: Lens.Lens' CreateClientVpnEndpoint Core.Text
createClientVpnEndpoint_clientCidrBlock = Lens.lens (\CreateClientVpnEndpoint' {clientCidrBlock} -> clientCidrBlock) (\s@CreateClientVpnEndpoint' {} a -> s {clientCidrBlock = a} :: CreateClientVpnEndpoint)

-- | The ARN of the server certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
createClientVpnEndpoint_serverCertificateArn :: Lens.Lens' CreateClientVpnEndpoint Core.Text
createClientVpnEndpoint_serverCertificateArn = Lens.lens (\CreateClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@CreateClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: CreateClientVpnEndpoint)

-- | Information about the authentication method to be used to authenticate
-- clients.
createClientVpnEndpoint_authenticationOptions :: Lens.Lens' CreateClientVpnEndpoint [ClientVpnAuthenticationRequest]
createClientVpnEndpoint_authenticationOptions = Lens.lens (\CreateClientVpnEndpoint' {authenticationOptions} -> authenticationOptions) (\s@CreateClientVpnEndpoint' {} a -> s {authenticationOptions = a} :: CreateClientVpnEndpoint) Core.. Lens._Coerce

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
createClientVpnEndpoint_connectionLogOptions :: Lens.Lens' CreateClientVpnEndpoint ConnectionLogOptions
createClientVpnEndpoint_connectionLogOptions = Lens.lens (\CreateClientVpnEndpoint' {connectionLogOptions} -> connectionLogOptions) (\s@CreateClientVpnEndpoint' {} a -> s {connectionLogOptions = a} :: CreateClientVpnEndpoint)

instance Core.AWSRequest CreateClientVpnEndpoint where
  type
    AWSResponse CreateClientVpnEndpoint =
      CreateClientVpnEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateClientVpnEndpointResponse'
            Core.<$> (x Core..@? "clientVpnEndpointId")
            Core.<*> (x Core..@? "status")
            Core.<*> (x Core..@? "dnsName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateClientVpnEndpoint

instance Core.NFData CreateClientVpnEndpoint

instance Core.ToHeaders CreateClientVpnEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateClientVpnEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery CreateClientVpnEndpoint where
  toQuery CreateClientVpnEndpoint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateClientVpnEndpoint" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Core.<$> securityGroupIds
          ),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "TransportProtocol" Core.=: transportProtocol,
        "ClientConnectOptions" Core.=: clientConnectOptions,
        Core.toQuery
          (Core.toQueryList "DnsServers" Core.<$> dnsServers),
        "VpnPort" Core.=: vpnPort,
        "Description" Core.=: description,
        "VpcId" Core.=: vpcId,
        "SelfServicePortal" Core.=: selfServicePortal,
        "ClientToken" Core.=: clientToken,
        "SplitTunnel" Core.=: splitTunnel,
        "ClientCidrBlock" Core.=: clientCidrBlock,
        "ServerCertificateArn" Core.=: serverCertificateArn,
        Core.toQueryList
          "Authentication"
          authenticationOptions,
        "ConnectionLogOptions" Core.=: connectionLogOptions
      ]

-- | /See:/ 'newCreateClientVpnEndpointResponse' smart constructor.
data CreateClientVpnEndpointResponse = CreateClientVpnEndpointResponse'
  { -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the Client VPN endpoint.
    status :: Core.Maybe ClientVpnEndpointStatus,
    -- | The DNS name to be used by clients when establishing their VPN session.
    dnsName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClientVpnEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'createClientVpnEndpointResponse_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'status', 'createClientVpnEndpointResponse_status' - The current state of the Client VPN endpoint.
--
-- 'dnsName', 'createClientVpnEndpointResponse_dnsName' - The DNS name to be used by clients when establishing their VPN session.
--
-- 'httpStatus', 'createClientVpnEndpointResponse_httpStatus' - The response's http status code.
newCreateClientVpnEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateClientVpnEndpointResponse
newCreateClientVpnEndpointResponse pHttpStatus_ =
  CreateClientVpnEndpointResponse'
    { clientVpnEndpointId =
        Core.Nothing,
      status = Core.Nothing,
      dnsName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Client VPN endpoint.
createClientVpnEndpointResponse_clientVpnEndpointId :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe Core.Text)
createClientVpnEndpointResponse_clientVpnEndpointId = Lens.lens (\CreateClientVpnEndpointResponse' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@CreateClientVpnEndpointResponse' {} a -> s {clientVpnEndpointId = a} :: CreateClientVpnEndpointResponse)

-- | The current state of the Client VPN endpoint.
createClientVpnEndpointResponse_status :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe ClientVpnEndpointStatus)
createClientVpnEndpointResponse_status = Lens.lens (\CreateClientVpnEndpointResponse' {status} -> status) (\s@CreateClientVpnEndpointResponse' {} a -> s {status = a} :: CreateClientVpnEndpointResponse)

-- | The DNS name to be used by clients when establishing their VPN session.
createClientVpnEndpointResponse_dnsName :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe Core.Text)
createClientVpnEndpointResponse_dnsName = Lens.lens (\CreateClientVpnEndpointResponse' {dnsName} -> dnsName) (\s@CreateClientVpnEndpointResponse' {} a -> s {dnsName = a} :: CreateClientVpnEndpointResponse)

-- | The response's http status code.
createClientVpnEndpointResponse_httpStatus :: Lens.Lens' CreateClientVpnEndpointResponse Core.Int
createClientVpnEndpointResponse_httpStatus = Lens.lens (\CreateClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: CreateClientVpnEndpointResponse)

instance Core.NFData CreateClientVpnEndpointResponse
