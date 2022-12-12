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
-- Module      : Amazonka.EC2.CreateClientVpnEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Client VPN endpoint. A Client VPN endpoint is the resource you
-- create and configure to enable and manage client VPN sessions. It is the
-- destination endpoint at which all client VPN sessions are terminated.
module Amazonka.EC2.CreateClientVpnEndpoint
  ( -- * Creating a Request
    CreateClientVpnEndpoint (..),
    newCreateClientVpnEndpoint,

    -- * Request Lenses
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_clientLoginBannerOptions,
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_sessionTimeoutHours,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,

    -- * Destructuring the Response
    CreateClientVpnEndpointResponse (..),
    newCreateClientVpnEndpointResponse,

    -- * Response Lenses
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_status,
    createClientVpnEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateClientVpnEndpoint' smart constructor.
data CreateClientVpnEndpoint = CreateClientVpnEndpoint'
  { -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Prelude.Maybe ClientConnectOptions,
    -- | Options for enabling a customizable text banner that will be displayed
    -- on Amazon Web Services provided clients when a VPN session is
    -- established.
    clientLoginBannerOptions :: Prelude.Maybe ClientLoginBannerOptions,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the Client VPN endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the DNS servers to be used for DNS resolution. A
    -- Client VPN endpoint can have up to two DNS servers. If no DNS server is
    -- specified, the DNS address configured on the device is used for the DNS
    -- server.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of one or more security groups to apply to the target network.
    -- You must also specify the ID of the VPC that contains the security
    -- groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify whether to enable the self-service portal for the Client VPN
    -- endpoint.
    --
    -- Default Value: @enabled@
    selfServicePortal :: Prelude.Maybe SelfServicePortal,
    -- | The maximum VPN session duration time in hours.
    --
    -- Valid values: @8 | 10 | 12 | 24@
    --
    -- Default value: @24@
    sessionTimeoutHours :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether split-tunnel is enabled on the Client VPN endpoint.
    --
    -- By default, split-tunnel on a VPN endpoint is disabled.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
    -- in the /Client VPN Administrator Guide/.
    splitTunnel :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the Client VPN endpoint during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The transport protocol to be used by the VPN session.
    --
    -- Default value: @udp@
    transportProtocol :: Prelude.Maybe TransportProtocol,
    -- | The ID of the VPC to associate with the Client VPN endpoint. If no
    -- security group IDs are specified in the request, the default security
    -- group for the VPC is applied.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The port number to assign to the Client VPN endpoint for TCP and UDP
    -- traffic.
    --
    -- Valid Values: @443@ | @1194@
    --
    -- Default Value: @443@
    vpnPort :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 address range, in CIDR notation, from which to assign client IP
    -- addresses. The address range cannot overlap with the local CIDR of the
    -- VPC in which the associated subnet is located, or the routes that you
    -- add manually. The address range cannot be changed after the Client VPN
    -- endpoint has been created. The CIDR block should be \/22 or greater.
    clientCidrBlock :: Prelude.Text,
    -- | The ARN of the server certificate. For more information, see the
    -- <https://docs.aws.amazon.com/acm/latest/userguide/ Certificate Manager User Guide>.
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
-- 'clientConnectOptions', 'createClientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'clientLoginBannerOptions', 'createClientVpnEndpoint_clientLoginBannerOptions' - Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
--
-- 'clientToken', 'createClientVpnEndpoint_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createClientVpnEndpoint_description' - A brief description of the Client VPN endpoint.
--
-- 'dnsServers', 'createClientVpnEndpoint_dnsServers' - Information about the DNS servers to be used for DNS resolution. A
-- Client VPN endpoint can have up to two DNS servers. If no DNS server is
-- specified, the DNS address configured on the device is used for the DNS
-- server.
--
-- 'dryRun', 'createClientVpnEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'securityGroupIds', 'createClientVpnEndpoint_securityGroupIds' - The IDs of one or more security groups to apply to the target network.
-- You must also specify the ID of the VPC that contains the security
-- groups.
--
-- 'selfServicePortal', 'createClientVpnEndpoint_selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- Default Value: @enabled@
--
-- 'sessionTimeoutHours', 'createClientVpnEndpoint_sessionTimeoutHours' - The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
--
-- 'splitTunnel', 'createClientVpnEndpoint_splitTunnel' - Indicates whether split-tunnel is enabled on the Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
--
-- 'tagSpecifications', 'createClientVpnEndpoint_tagSpecifications' - The tags to apply to the Client VPN endpoint during creation.
--
-- 'transportProtocol', 'createClientVpnEndpoint_transportProtocol' - The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
--
-- 'vpcId', 'createClientVpnEndpoint_vpcId' - The ID of the VPC to associate with the Client VPN endpoint. If no
-- security group IDs are specified in the request, the default security
-- group for the VPC is applied.
--
-- 'vpnPort', 'createClientVpnEndpoint_vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
--
-- 'clientCidrBlock', 'createClientVpnEndpoint_clientCidrBlock' - The IPv4 address range, in CIDR notation, from which to assign client IP
-- addresses. The address range cannot overlap with the local CIDR of the
-- VPC in which the associated subnet is located, or the routes that you
-- add manually. The address range cannot be changed after the Client VPN
-- endpoint has been created. The CIDR block should be \/22 or greater.
--
-- 'serverCertificateArn', 'createClientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ Certificate Manager User Guide>.
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
      { clientConnectOptions =
          Prelude.Nothing,
        clientLoginBannerOptions = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        dnsServers = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        selfServicePortal = Prelude.Nothing,
        sessionTimeoutHours = Prelude.Nothing,
        splitTunnel = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        transportProtocol = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        vpnPort = Prelude.Nothing,
        clientCidrBlock = pClientCidrBlock_,
        serverCertificateArn = pServerCertificateArn_,
        authenticationOptions = Prelude.mempty,
        connectionLogOptions = pConnectionLogOptions_
      }

-- | The options for managing connection authorization for new client
-- connections.
createClientVpnEndpoint_clientConnectOptions :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe ClientConnectOptions)
createClientVpnEndpoint_clientConnectOptions = Lens.lens (\CreateClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@CreateClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: CreateClientVpnEndpoint)

-- | Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
createClientVpnEndpoint_clientLoginBannerOptions :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe ClientLoginBannerOptions)
createClientVpnEndpoint_clientLoginBannerOptions = Lens.lens (\CreateClientVpnEndpoint' {clientLoginBannerOptions} -> clientLoginBannerOptions) (\s@CreateClientVpnEndpoint' {} a -> s {clientLoginBannerOptions = a} :: CreateClientVpnEndpoint)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createClientVpnEndpoint_clientToken :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_clientToken = Lens.lens (\CreateClientVpnEndpoint' {clientToken} -> clientToken) (\s@CreateClientVpnEndpoint' {} a -> s {clientToken = a} :: CreateClientVpnEndpoint)

-- | A brief description of the Client VPN endpoint.
createClientVpnEndpoint_description :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_description = Lens.lens (\CreateClientVpnEndpoint' {description} -> description) (\s@CreateClientVpnEndpoint' {} a -> s {description = a} :: CreateClientVpnEndpoint)

-- | Information about the DNS servers to be used for DNS resolution. A
-- Client VPN endpoint can have up to two DNS servers. If no DNS server is
-- specified, the DNS address configured on the device is used for the DNS
-- server.
createClientVpnEndpoint_dnsServers :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
createClientVpnEndpoint_dnsServers = Lens.lens (\CreateClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@CreateClientVpnEndpoint' {} a -> s {dnsServers = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createClientVpnEndpoint_dryRun :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
createClientVpnEndpoint_dryRun = Lens.lens (\CreateClientVpnEndpoint' {dryRun} -> dryRun) (\s@CreateClientVpnEndpoint' {} a -> s {dryRun = a} :: CreateClientVpnEndpoint)

-- | The IDs of one or more security groups to apply to the target network.
-- You must also specify the ID of the VPC that contains the security
-- groups.
createClientVpnEndpoint_securityGroupIds :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
createClientVpnEndpoint_securityGroupIds = Lens.lens (\CreateClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- Default Value: @enabled@
createClientVpnEndpoint_selfServicePortal :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe SelfServicePortal)
createClientVpnEndpoint_selfServicePortal = Lens.lens (\CreateClientVpnEndpoint' {selfServicePortal} -> selfServicePortal) (\s@CreateClientVpnEndpoint' {} a -> s {selfServicePortal = a} :: CreateClientVpnEndpoint)

-- | The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
createClientVpnEndpoint_sessionTimeoutHours :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Int)
createClientVpnEndpoint_sessionTimeoutHours = Lens.lens (\CreateClientVpnEndpoint' {sessionTimeoutHours} -> sessionTimeoutHours) (\s@CreateClientVpnEndpoint' {} a -> s {sessionTimeoutHours = a} :: CreateClientVpnEndpoint)

-- | Indicates whether split-tunnel is enabled on the Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
createClientVpnEndpoint_splitTunnel :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
createClientVpnEndpoint_splitTunnel = Lens.lens (\CreateClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@CreateClientVpnEndpoint' {} a -> s {splitTunnel = a} :: CreateClientVpnEndpoint)

-- | The tags to apply to the Client VPN endpoint during creation.
createClientVpnEndpoint_tagSpecifications :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe [TagSpecification])
createClientVpnEndpoint_tagSpecifications = Lens.lens (\CreateClientVpnEndpoint' {tagSpecifications} -> tagSpecifications) (\s@CreateClientVpnEndpoint' {} a -> s {tagSpecifications = a} :: CreateClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
createClientVpnEndpoint_transportProtocol :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe TransportProtocol)
createClientVpnEndpoint_transportProtocol = Lens.lens (\CreateClientVpnEndpoint' {transportProtocol} -> transportProtocol) (\s@CreateClientVpnEndpoint' {} a -> s {transportProtocol = a} :: CreateClientVpnEndpoint)

-- | The ID of the VPC to associate with the Client VPN endpoint. If no
-- security group IDs are specified in the request, the default security
-- group for the VPC is applied.
createClientVpnEndpoint_vpcId :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Text)
createClientVpnEndpoint_vpcId = Lens.lens (\CreateClientVpnEndpoint' {vpcId} -> vpcId) (\s@CreateClientVpnEndpoint' {} a -> s {vpcId = a} :: CreateClientVpnEndpoint)

-- | The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
createClientVpnEndpoint_vpnPort :: Lens.Lens' CreateClientVpnEndpoint (Prelude.Maybe Prelude.Int)
createClientVpnEndpoint_vpnPort = Lens.lens (\CreateClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@CreateClientVpnEndpoint' {} a -> s {vpnPort = a} :: CreateClientVpnEndpoint)

-- | The IPv4 address range, in CIDR notation, from which to assign client IP
-- addresses. The address range cannot overlap with the local CIDR of the
-- VPC in which the associated subnet is located, or the routes that you
-- add manually. The address range cannot be changed after the Client VPN
-- endpoint has been created. The CIDR block should be \/22 or greater.
createClientVpnEndpoint_clientCidrBlock :: Lens.Lens' CreateClientVpnEndpoint Prelude.Text
createClientVpnEndpoint_clientCidrBlock = Lens.lens (\CreateClientVpnEndpoint' {clientCidrBlock} -> clientCidrBlock) (\s@CreateClientVpnEndpoint' {} a -> s {clientCidrBlock = a} :: CreateClientVpnEndpoint)

-- | The ARN of the server certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ Certificate Manager User Guide>.
createClientVpnEndpoint_serverCertificateArn :: Lens.Lens' CreateClientVpnEndpoint Prelude.Text
createClientVpnEndpoint_serverCertificateArn = Lens.lens (\CreateClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@CreateClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: CreateClientVpnEndpoint)

-- | Information about the authentication method to be used to authenticate
-- clients.
createClientVpnEndpoint_authenticationOptions :: Lens.Lens' CreateClientVpnEndpoint [ClientVpnAuthenticationRequest]
createClientVpnEndpoint_authenticationOptions = Lens.lens (\CreateClientVpnEndpoint' {authenticationOptions} -> authenticationOptions) (\s@CreateClientVpnEndpoint' {} a -> s {authenticationOptions = a} :: CreateClientVpnEndpoint) Prelude.. Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateClientVpnEndpointResponse'
            Prelude.<$> (x Data..@? "clientVpnEndpointId")
            Prelude.<*> (x Data..@? "dnsName")
            Prelude.<*> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClientVpnEndpoint where
  hashWithSalt _salt CreateClientVpnEndpoint' {..} =
    _salt `Prelude.hashWithSalt` clientConnectOptions
      `Prelude.hashWithSalt` clientLoginBannerOptions
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dnsServers
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` selfServicePortal
      `Prelude.hashWithSalt` sessionTimeoutHours
      `Prelude.hashWithSalt` splitTunnel
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` transportProtocol
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpnPort
      `Prelude.hashWithSalt` clientCidrBlock
      `Prelude.hashWithSalt` serverCertificateArn
      `Prelude.hashWithSalt` authenticationOptions
      `Prelude.hashWithSalt` connectionLogOptions

instance Prelude.NFData CreateClientVpnEndpoint where
  rnf CreateClientVpnEndpoint' {..} =
    Prelude.rnf clientConnectOptions
      `Prelude.seq` Prelude.rnf clientLoginBannerOptions
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dnsServers
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf selfServicePortal
      `Prelude.seq` Prelude.rnf sessionTimeoutHours
      `Prelude.seq` Prelude.rnf splitTunnel
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transportProtocol
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpnPort
      `Prelude.seq` Prelude.rnf clientCidrBlock
      `Prelude.seq` Prelude.rnf serverCertificateArn
      `Prelude.seq` Prelude.rnf authenticationOptions
      `Prelude.seq` Prelude.rnf connectionLogOptions

instance Data.ToHeaders CreateClientVpnEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateClientVpnEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateClientVpnEndpoint where
  toQuery CreateClientVpnEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateClientVpnEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientConnectOptions" Data.=: clientConnectOptions,
        "ClientLoginBannerOptions"
          Data.=: clientLoginBannerOptions,
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        Data.toQuery
          ( Data.toQueryList "DnsServers"
              Prelude.<$> dnsServers
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "SelfServicePortal" Data.=: selfServicePortal,
        "SessionTimeoutHours" Data.=: sessionTimeoutHours,
        "SplitTunnel" Data.=: splitTunnel,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TransportProtocol" Data.=: transportProtocol,
        "VpcId" Data.=: vpcId,
        "VpnPort" Data.=: vpnPort,
        "ClientCidrBlock" Data.=: clientCidrBlock,
        "ServerCertificateArn" Data.=: serverCertificateArn,
        Data.toQueryList
          "Authentication"
          authenticationOptions,
        "ConnectionLogOptions" Data.=: connectionLogOptions
      ]

-- | /See:/ 'newCreateClientVpnEndpointResponse' smart constructor.
data CreateClientVpnEndpointResponse = CreateClientVpnEndpointResponse'
  { -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The DNS name to be used by clients when establishing their VPN session.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Client VPN endpoint.
    status :: Prelude.Maybe ClientVpnEndpointStatus,
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
-- 'dnsName', 'createClientVpnEndpointResponse_dnsName' - The DNS name to be used by clients when establishing their VPN session.
--
-- 'status', 'createClientVpnEndpointResponse_status' - The current state of the Client VPN endpoint.
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
      dnsName = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Client VPN endpoint.
createClientVpnEndpointResponse_clientVpnEndpointId :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe Prelude.Text)
createClientVpnEndpointResponse_clientVpnEndpointId = Lens.lens (\CreateClientVpnEndpointResponse' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@CreateClientVpnEndpointResponse' {} a -> s {clientVpnEndpointId = a} :: CreateClientVpnEndpointResponse)

-- | The DNS name to be used by clients when establishing their VPN session.
createClientVpnEndpointResponse_dnsName :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe Prelude.Text)
createClientVpnEndpointResponse_dnsName = Lens.lens (\CreateClientVpnEndpointResponse' {dnsName} -> dnsName) (\s@CreateClientVpnEndpointResponse' {} a -> s {dnsName = a} :: CreateClientVpnEndpointResponse)

-- | The current state of the Client VPN endpoint.
createClientVpnEndpointResponse_status :: Lens.Lens' CreateClientVpnEndpointResponse (Prelude.Maybe ClientVpnEndpointStatus)
createClientVpnEndpointResponse_status = Lens.lens (\CreateClientVpnEndpointResponse' {status} -> status) (\s@CreateClientVpnEndpointResponse' {} a -> s {status = a} :: CreateClientVpnEndpointResponse)

-- | The response's http status code.
createClientVpnEndpointResponse_httpStatus :: Lens.Lens' CreateClientVpnEndpointResponse Prelude.Int
createClientVpnEndpointResponse_httpStatus = Lens.lens (\CreateClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: CreateClientVpnEndpointResponse)

instance
  Prelude.NFData
    CreateClientVpnEndpointResponse
  where
  rnf CreateClientVpnEndpointResponse' {..} =
    Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
