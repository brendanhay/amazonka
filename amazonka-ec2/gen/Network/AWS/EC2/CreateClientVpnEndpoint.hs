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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateClientVpnEndpoint' smart constructor.
data CreateClientVpnEndpoint = CreateClientVpnEndpoint'
  { -- | The IDs of one or more security groups to apply to the target network.
    -- You must also specify the ID of the VPC that contains the security
    -- groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to apply to the Client VPN endpoint during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The transport protocol to be used by the VPN session.
    --
    -- Default value: @udp@
    transportProtocol :: Prelude.Maybe TransportProtocol,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Prelude.Maybe ClientConnectOptions,
    -- | Information about the DNS servers to be used for DNS resolution. A
    -- Client VPN endpoint can have up to two DNS servers. If no DNS server is
    -- specified, the DNS address configured on the device is used for the DNS
    -- server.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | The port number to assign to the Client VPN endpoint for TCP and UDP
    -- traffic.
    --
    -- Valid Values: @443@ | @1194@
    --
    -- Default Value: @443@
    vpnPort :: Prelude.Maybe Prelude.Int,
    -- | A brief description of the Client VPN endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC to associate with the Client VPN endpoint. If no
    -- security group IDs are specified in the request, the default security
    -- group for the VPC is applied.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Specify whether to enable the self-service portal for the Client VPN
    -- endpoint.
    --
    -- Default Value: @enabled@
    selfServicePortal :: Prelude.Maybe SelfServicePortal,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether split-tunnel is enabled on the AWS Client VPN
    -- endpoint.
    --
    -- By default, split-tunnel on a VPN endpoint is disabled.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
    -- in the /AWS Client VPN Administrator Guide/.
    splitTunnel :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 address range, in CIDR notation, from which to assign client IP
    -- addresses. The address range cannot overlap with the local CIDR of the
    -- VPC in which the associated subnet is located, or the routes that you
    -- add manually. The address range cannot be changed after the Client VPN
    -- endpoint has been created. The CIDR block should be \/22 or greater.
    clientCidrBlock :: Prelude.Text,
    -- | The ARN of the server certificate. For more information, see the
    -- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
    serverCertificateArn :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'serverCertificateArn'
  Prelude.Text ->
  -- | 'connectionLogOptions'
  ConnectionLogOptions ->
  CreateClientVpnEndpoint
newCreateClientVpnEndpoint
  pClientCidrBlock_
  pServerCertificateArn_
  pConnectionLogOptions_ =
    CreateClientVpnEndpoint'
      { securityGroupIds =
          Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        transportProtocol = Prelude.Nothing,
        clientConnectOptions = Prelude.Nothing,
        dnsServers = Prelude.Nothing,
        vpnPort = Prelude.Nothing,
        description = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        selfServicePortal = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        splitTunnel = Prelude.Nothing,
        clientCidrBlock = pClientCidrBlock_,
        serverCertificateArn = pServerCertificateArn_,
        authenticationOptions = Prelude.mempty,
        connectionLogOptions = pConnectionLogOptions_
      }

-- | The IDs of one or more security groups to apply to the target network.
-- You must also specify the ID of the VPC that contains the security
-- groups.
createClientVpnEndpoint_securityGroupIds :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
createClientVpnEndpoint_securityGroupIds = Lens.lens (\CreateClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The tags to apply to the Client VPN endpoint during creation.
createClientVpnEndpoint_tagSpecifications :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [TagSpecification])
createClientVpnEndpoint_tagSpecifications = Lens.lens (\CreateClientVpnEndpoint' {tagSpecifications} -> tagSpecifications) (\s@CreateClientVpnEndpoint' {} a -> s {tagSpecifications = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createClientVpnEndpoint_dryRun :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
createClientVpnEndpoint_dryRun = Lens.lens (\CreateClientVpnEndpoint' {dryRun} -> dryRun) (\s@CreateClientVpnEndpoint' {} a -> s {dryRun = a} :: CreateClientVpnEndpoint)

-- | The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
createClientVpnEndpoint_transportProtocol :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe TransportProtocol)
createClientVpnEndpoint_transportProtocol = Lens.lens (\CreateClientVpnEndpoint' {transportProtocol} -> transportProtocol) (\s@CreateClientVpnEndpoint' {} a -> s {transportProtocol = a} :: CreateClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
createClientVpnEndpoint_clientConnectOptions :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe ClientConnectOptions)
createClientVpnEndpoint_clientConnectOptions = Lens.lens (\CreateClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@CreateClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: CreateClientVpnEndpoint)

-- | Information about the DNS servers to be used for DNS resolution. A
-- Client VPN endpoint can have up to two DNS servers. If no DNS server is
-- specified, the DNS address configured on the device is used for the DNS
-- server.
createClientVpnEndpoint_dnsServers :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
createClientVpnEndpoint_dnsServers = Lens.lens (\CreateClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@CreateClientVpnEndpoint' {} a -> s {dnsServers = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
createClientVpnEndpoint_vpnPort :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Int)
createClientVpnEndpoint_vpnPort = Lens.lens (\CreateClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@CreateClientVpnEndpoint' {} a -> s {vpnPort = a} :: CreateClientVpnEndpoint)

-- | A brief description of the Client VPN endpoint.
createClientVpnEndpoint_description :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_description = Lens.lens (\CreateClientVpnEndpoint' {description} -> description) (\s@CreateClientVpnEndpoint' {} a -> s {description = a} :: CreateClientVpnEndpoint)

-- | The ID of the VPC to associate with the Client VPN endpoint. If no
-- security group IDs are specified in the request, the default security
-- group for the VPC is applied.
createClientVpnEndpoint_vpcId :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_vpcId = Lens.lens (\CreateClientVpnEndpoint' {vpcId} -> vpcId) (\s@CreateClientVpnEndpoint' {} a -> s {vpcId = a} :: CreateClientVpnEndpoint)

-- | Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- Default Value: @enabled@
createClientVpnEndpoint_selfServicePortal :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe SelfServicePortal)
createClientVpnEndpoint_selfServicePortal = Lens.lens (\CreateClientVpnEndpoint' {selfServicePortal} -> selfServicePortal) (\s@CreateClientVpnEndpoint' {} a -> s {selfServicePortal = a} :: CreateClientVpnEndpoint)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createClientVpnEndpoint_clientToken :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_clientToken = Lens.lens (\CreateClientVpnEndpoint' {clientToken} -> clientToken) (\s@CreateClientVpnEndpoint' {} a -> s {clientToken = a} :: CreateClientVpnEndpoint)

-- | Indicates whether split-tunnel is enabled on the AWS Client VPN
-- endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
createClientVpnEndpoint_splitTunnel :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
createClientVpnEndpoint_splitTunnel = Lens.lens (\CreateClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@CreateClientVpnEndpoint' {} a -> s {splitTunnel = a} :: CreateClientVpnEndpoint)

-- | The IPv4 address range, in CIDR notation, from which to assign client IP
-- addresses. The address range cannot overlap with the local CIDR of the
-- VPC in which the associated subnet is located, or the routes that you
-- add manually. The address range cannot be changed after the Client VPN
-- endpoint has been created. The CIDR block should be \/22 or greater.
createClientVpnEndpoint_clientCidrBlock :: Lens.Lens' CreateClientVpnEndpoint Prelude.Text
createClientVpnEndpoint_clientCidrBlock = Lens.lens (\CreateClientVpnEndpoint' {clientCidrBlock} -> clientCidrBlock) (\s@CreateClientVpnEndpoint' {} a -> s {clientCidrBlock = a} :: CreateClientVpnEndpoint)

-- | The ARN of the server certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
createClientVpnEndpoint_serverCertificateArn :: Lens.Lens' CreateClientVpnEndpoint Prelude.Text
createClientVpnEndpoint_serverCertificateArn = Lens.lens (\CreateClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@CreateClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: CreateClientVpnEndpoint)

-- | Information about the authentication method to be used to authenticate
-- clients.
createClientVpnEndpoint_authenticationOptions :: Lens.Lens' CreateClientVpnEndpoint [ClientVpnAuthenticationRequest]
createClientVpnEndpoint_authenticationOptions = Lens.lens (\CreateClientVpnEndpoint' {authenticationOptions} -> authenticationOptions) (\s@CreateClientVpnEndpoint' {} a -> s {authenticationOptions = a} :: CreateClientVpnEndpoint) Prelude.. Lens._Coerce

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
            Prelude.<$> (x Core..@? "clientVpnEndpointId")
            Prelude.<*> (x Core..@? "status")
            Prelude.<*> (x Core..@? "dnsName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClientVpnEndpoint

instance Prelude.NFData CreateClientVpnEndpoint

instance Core.ToHeaders CreateClientVpnEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateClientVpnEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateClientVpnEndpoint where
  toQuery CreateClientVpnEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateClientVpnEndpoint" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "TransportProtocol" Core.=: transportProtocol,
        "ClientConnectOptions" Core.=: clientConnectOptions,
        Core.toQuery
          ( Core.toQueryList "DnsServers"
              Prelude.<$> dnsServers
          ),
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
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Client VPN endpoint.
    status :: Prelude.Maybe ClientVpnEndpointStatus,
    -- | The DNS name to be used by clients when establishing their VPN session.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateClientVpnEndpointResponse
newCreateClientVpnEndpointResponse pHttpStatus_ =
  CreateClientVpnEndpointResponse'
    { clientVpnEndpointId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Client VPN endpoint.
createClientVpnEndpointResponse_clientVpnEndpointId :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe Prelude.Text)
createClientVpnEndpointResponse_clientVpnEndpointId = Lens.lens (\CreateClientVpnEndpointResponse' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@CreateClientVpnEndpointResponse' {} a -> s {clientVpnEndpointId = a} :: CreateClientVpnEndpointResponse)

-- | The current state of the Client VPN endpoint.
createClientVpnEndpointResponse_status :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe ClientVpnEndpointStatus)
createClientVpnEndpointResponse_status = Lens.lens (\CreateClientVpnEndpointResponse' {status} -> status) (\s@CreateClientVpnEndpointResponse' {} a -> s {status = a} :: CreateClientVpnEndpointResponse)

-- | The DNS name to be used by clients when establishing their VPN session.
createClientVpnEndpointResponse_dnsName :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe Prelude.Text)
createClientVpnEndpointResponse_dnsName = Lens.lens (\CreateClientVpnEndpointResponse' {dnsName} -> dnsName) (\s@CreateClientVpnEndpointResponse' {} a -> s {dnsName = a} :: CreateClientVpnEndpointResponse)

-- | The response's http status code.
createClientVpnEndpointResponse_httpStatus :: Lens.Lens' CreateClientVpnEndpointResponse Prelude.Int
createClientVpnEndpointResponse_httpStatus = Lens.lens (\CreateClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: CreateClientVpnEndpointResponse)

instance
  Prelude.NFData
    CreateClientVpnEndpointResponse
