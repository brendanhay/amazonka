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
-- Module      : Amazonka.EC2.ModifyClientVpnEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Client VPN endpoint. Modifying the DNS server
-- resets existing client connections.
module Amazonka.EC2.ModifyClientVpnEndpoint
  ( -- * Creating a Request
    ModifyClientVpnEndpoint (..),
    newModifyClientVpnEndpoint,

    -- * Request Lenses
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_sessionTimeoutHours,
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_clientLoginBannerOptions,
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_vpnPort,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_clientVpnEndpointId,

    -- * Destructuring the Response
    ModifyClientVpnEndpointResponse (..),
    newModifyClientVpnEndpointResponse,

    -- * Response Lenses
    modifyClientVpnEndpointResponse_return,
    modifyClientVpnEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyClientVpnEndpoint' smart constructor.
data ModifyClientVpnEndpoint = ModifyClientVpnEndpoint'
  { -- | The ARN of the server certificate to be used. The server certificate
    -- must be provisioned in Certificate Manager (ACM).
    serverCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum VPN session duration time in hours.
    --
    -- Valid values: @8 | 10 | 12 | 24@
    --
    -- Default value: @24@
    sessionTimeoutHours :: Prelude.Maybe Prelude.Int,
    -- | The IDs of one or more security groups to apply to the target network.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Information about the DNS servers to be used by Client VPN connections.
    -- A Client VPN endpoint can have up to two DNS servers.
    dnsServers :: Prelude.Maybe DnsServersOptionsModifyStructure,
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
    connectionLogOptions :: Prelude.Maybe ConnectionLogOptions,
    -- | A brief description of the Client VPN endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Options for enabling a customizable text banner that will be displayed
    -- on Amazon Web Services provided clients when a VPN session is
    -- established.
    clientLoginBannerOptions :: Prelude.Maybe ClientLoginBannerOptions,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Prelude.Maybe ClientConnectOptions,
    -- | Indicates whether the VPN is split-tunnel.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
    -- in the /Client VPN Administrator Guide/.
    splitTunnel :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC to associate with the Client VPN endpoint.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The port number to assign to the Client VPN endpoint for TCP and UDP
    -- traffic.
    --
    -- Valid Values: @443@ | @1194@
    --
    -- Default Value: @443@
    vpnPort :: Prelude.Maybe Prelude.Int,
    -- | Specify whether to enable the self-service portal for the Client VPN
    -- endpoint.
    selfServicePortal :: Prelude.Maybe SelfServicePortal,
    -- | The ID of the Client VPN endpoint to modify.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateArn', 'modifyClientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate to be used. The server certificate
-- must be provisioned in Certificate Manager (ACM).
--
-- 'sessionTimeoutHours', 'modifyClientVpnEndpoint_sessionTimeoutHours' - The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
--
-- 'securityGroupIds', 'modifyClientVpnEndpoint_securityGroupIds' - The IDs of one or more security groups to apply to the target network.
--
-- 'dnsServers', 'modifyClientVpnEndpoint_dnsServers' - Information about the DNS servers to be used by Client VPN connections.
-- A Client VPN endpoint can have up to two DNS servers.
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
-- 'description', 'modifyClientVpnEndpoint_description' - A brief description of the Client VPN endpoint.
--
-- 'dryRun', 'modifyClientVpnEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientLoginBannerOptions', 'modifyClientVpnEndpoint_clientLoginBannerOptions' - Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
--
-- 'clientConnectOptions', 'modifyClientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'splitTunnel', 'modifyClientVpnEndpoint_splitTunnel' - Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
--
-- 'vpcId', 'modifyClientVpnEndpoint_vpcId' - The ID of the VPC to associate with the Client VPN endpoint.
--
-- 'vpnPort', 'modifyClientVpnEndpoint_vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
--
-- 'selfServicePortal', 'modifyClientVpnEndpoint_selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
--
-- 'clientVpnEndpointId', 'modifyClientVpnEndpoint_clientVpnEndpointId' - The ID of the Client VPN endpoint to modify.
newModifyClientVpnEndpoint ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  ModifyClientVpnEndpoint
newModifyClientVpnEndpoint pClientVpnEndpointId_ =
  ModifyClientVpnEndpoint'
    { serverCertificateArn =
        Prelude.Nothing,
      sessionTimeoutHours = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      dnsServers = Prelude.Nothing,
      connectionLogOptions = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      clientLoginBannerOptions = Prelude.Nothing,
      clientConnectOptions = Prelude.Nothing,
      splitTunnel = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpnPort = Prelude.Nothing,
      selfServicePortal = Prelude.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | The ARN of the server certificate to be used. The server certificate
-- must be provisioned in Certificate Manager (ACM).
modifyClientVpnEndpoint_serverCertificateArn :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Text)
modifyClientVpnEndpoint_serverCertificateArn = Lens.lens (\ModifyClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@ModifyClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: ModifyClientVpnEndpoint)

-- | The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
modifyClientVpnEndpoint_sessionTimeoutHours :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Int)
modifyClientVpnEndpoint_sessionTimeoutHours = Lens.lens (\ModifyClientVpnEndpoint' {sessionTimeoutHours} -> sessionTimeoutHours) (\s@ModifyClientVpnEndpoint' {} a -> s {sessionTimeoutHours = a} :: ModifyClientVpnEndpoint)

-- | The IDs of one or more security groups to apply to the target network.
modifyClientVpnEndpoint_securityGroupIds :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
modifyClientVpnEndpoint_securityGroupIds = Lens.lens (\ModifyClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@ModifyClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: ModifyClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Information about the DNS servers to be used by Client VPN connections.
-- A Client VPN endpoint can have up to two DNS servers.
modifyClientVpnEndpoint_dnsServers :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe DnsServersOptionsModifyStructure)
modifyClientVpnEndpoint_dnsServers = Lens.lens (\ModifyClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@ModifyClientVpnEndpoint' {} a -> s {dnsServers = a} :: ModifyClientVpnEndpoint)

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
modifyClientVpnEndpoint_connectionLogOptions :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe ConnectionLogOptions)
modifyClientVpnEndpoint_connectionLogOptions = Lens.lens (\ModifyClientVpnEndpoint' {connectionLogOptions} -> connectionLogOptions) (\s@ModifyClientVpnEndpoint' {} a -> s {connectionLogOptions = a} :: ModifyClientVpnEndpoint)

-- | A brief description of the Client VPN endpoint.
modifyClientVpnEndpoint_description :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Text)
modifyClientVpnEndpoint_description = Lens.lens (\ModifyClientVpnEndpoint' {description} -> description) (\s@ModifyClientVpnEndpoint' {} a -> s {description = a} :: ModifyClientVpnEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyClientVpnEndpoint_dryRun :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
modifyClientVpnEndpoint_dryRun = Lens.lens (\ModifyClientVpnEndpoint' {dryRun} -> dryRun) (\s@ModifyClientVpnEndpoint' {} a -> s {dryRun = a} :: ModifyClientVpnEndpoint)

-- | Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
modifyClientVpnEndpoint_clientLoginBannerOptions :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe ClientLoginBannerOptions)
modifyClientVpnEndpoint_clientLoginBannerOptions = Lens.lens (\ModifyClientVpnEndpoint' {clientLoginBannerOptions} -> clientLoginBannerOptions) (\s@ModifyClientVpnEndpoint' {} a -> s {clientLoginBannerOptions = a} :: ModifyClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
modifyClientVpnEndpoint_clientConnectOptions :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe ClientConnectOptions)
modifyClientVpnEndpoint_clientConnectOptions = Lens.lens (\ModifyClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@ModifyClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: ModifyClientVpnEndpoint)

-- | Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
modifyClientVpnEndpoint_splitTunnel :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
modifyClientVpnEndpoint_splitTunnel = Lens.lens (\ModifyClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@ModifyClientVpnEndpoint' {} a -> s {splitTunnel = a} :: ModifyClientVpnEndpoint)

-- | The ID of the VPC to associate with the Client VPN endpoint.
modifyClientVpnEndpoint_vpcId :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Text)
modifyClientVpnEndpoint_vpcId = Lens.lens (\ModifyClientVpnEndpoint' {vpcId} -> vpcId) (\s@ModifyClientVpnEndpoint' {} a -> s {vpcId = a} :: ModifyClientVpnEndpoint)

-- | The port number to assign to the Client VPN endpoint for TCP and UDP
-- traffic.
--
-- Valid Values: @443@ | @1194@
--
-- Default Value: @443@
modifyClientVpnEndpoint_vpnPort :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe Prelude.Int)
modifyClientVpnEndpoint_vpnPort = Lens.lens (\ModifyClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@ModifyClientVpnEndpoint' {} a -> s {vpnPort = a} :: ModifyClientVpnEndpoint)

-- | Specify whether to enable the self-service portal for the Client VPN
-- endpoint.
modifyClientVpnEndpoint_selfServicePortal :: Lens.Lens' ModifyClientVpnEndpoint (Prelude.Maybe SelfServicePortal)
modifyClientVpnEndpoint_selfServicePortal = Lens.lens (\ModifyClientVpnEndpoint' {selfServicePortal} -> selfServicePortal) (\s@ModifyClientVpnEndpoint' {} a -> s {selfServicePortal = a} :: ModifyClientVpnEndpoint)

-- | The ID of the Client VPN endpoint to modify.
modifyClientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' ModifyClientVpnEndpoint Prelude.Text
modifyClientVpnEndpoint_clientVpnEndpointId = Lens.lens (\ModifyClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ModifyClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: ModifyClientVpnEndpoint)

instance Core.AWSRequest ModifyClientVpnEndpoint where
  type
    AWSResponse ModifyClientVpnEndpoint =
      ModifyClientVpnEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyClientVpnEndpointResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClientVpnEndpoint where
  hashWithSalt _salt ModifyClientVpnEndpoint' {..} =
    _salt `Prelude.hashWithSalt` serverCertificateArn
      `Prelude.hashWithSalt` sessionTimeoutHours
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` dnsServers
      `Prelude.hashWithSalt` connectionLogOptions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` clientLoginBannerOptions
      `Prelude.hashWithSalt` clientConnectOptions
      `Prelude.hashWithSalt` splitTunnel
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpnPort
      `Prelude.hashWithSalt` selfServicePortal
      `Prelude.hashWithSalt` clientVpnEndpointId

instance Prelude.NFData ModifyClientVpnEndpoint where
  rnf ModifyClientVpnEndpoint' {..} =
    Prelude.rnf serverCertificateArn
      `Prelude.seq` Prelude.rnf sessionTimeoutHours
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf dnsServers
      `Prelude.seq` Prelude.rnf connectionLogOptions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf clientLoginBannerOptions
      `Prelude.seq` Prelude.rnf clientConnectOptions
      `Prelude.seq` Prelude.rnf splitTunnel
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpnPort
      `Prelude.seq` Prelude.rnf selfServicePortal
      `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance Data.ToHeaders ModifyClientVpnEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyClientVpnEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyClientVpnEndpoint where
  toQuery ModifyClientVpnEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyClientVpnEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ServerCertificateArn" Data.=: serverCertificateArn,
        "SessionTimeoutHours" Data.=: sessionTimeoutHours,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "DnsServers" Data.=: dnsServers,
        "ConnectionLogOptions" Data.=: connectionLogOptions,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "ClientLoginBannerOptions"
          Data.=: clientLoginBannerOptions,
        "ClientConnectOptions" Data.=: clientConnectOptions,
        "SplitTunnel" Data.=: splitTunnel,
        "VpcId" Data.=: vpcId,
        "VpnPort" Data.=: vpnPort,
        "SelfServicePortal" Data.=: selfServicePortal,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId
      ]

-- | /See:/ 'newModifyClientVpnEndpointResponse' smart constructor.
data ModifyClientVpnEndpointResponse = ModifyClientVpnEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyClientVpnEndpointResponse
newModifyClientVpnEndpointResponse pHttpStatus_ =
  ModifyClientVpnEndpointResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyClientVpnEndpointResponse_return :: Lens.Lens' ModifyClientVpnEndpointResponse (Prelude.Maybe Prelude.Bool)
modifyClientVpnEndpointResponse_return = Lens.lens (\ModifyClientVpnEndpointResponse' {return'} -> return') (\s@ModifyClientVpnEndpointResponse' {} a -> s {return' = a} :: ModifyClientVpnEndpointResponse)

-- | The response's http status code.
modifyClientVpnEndpointResponse_httpStatus :: Lens.Lens' ModifyClientVpnEndpointResponse Prelude.Int
modifyClientVpnEndpointResponse_httpStatus = Lens.lens (\ModifyClientVpnEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyClientVpnEndpointResponse' {} a -> s {httpStatus = a} :: ModifyClientVpnEndpointResponse)

instance
  Prelude.NFData
    ModifyClientVpnEndpointResponse
  where
  rnf ModifyClientVpnEndpointResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
