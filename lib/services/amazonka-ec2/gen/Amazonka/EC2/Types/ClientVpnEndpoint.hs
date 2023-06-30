{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.ClientVpnEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AssociatedTargetNetwork
import Amazonka.EC2.Types.ClientConnectResponseOptions
import Amazonka.EC2.Types.ClientLoginBannerResponseOptions
import Amazonka.EC2.Types.ClientVpnAuthentication
import Amazonka.EC2.Types.ClientVpnEndpointStatus
import Amazonka.EC2.Types.ConnectionLogResponseOptions
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransportProtocol
import Amazonka.EC2.Types.VpnProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a Client VPN endpoint.
--
-- /See:/ 'newClientVpnEndpoint' smart constructor.
data ClientVpnEndpoint = ClientVpnEndpoint'
  { -- | Information about the associated target networks. A target network is a
    -- subnet in a VPC.
    associatedTargetNetworks :: Prelude.Maybe [AssociatedTargetNetwork],
    -- | Information about the authentication method used by the Client VPN
    -- endpoint.
    authenticationOptions :: Prelude.Maybe [ClientVpnAuthentication],
    -- | The IPv4 address range, in CIDR notation, from which client IP addresses
    -- are assigned.
    clientCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Prelude.Maybe ClientConnectResponseOptions,
    -- | Options for enabling a customizable text banner that will be displayed
    -- on Amazon Web Services provided clients when a VPN session is
    -- established.
    clientLoginBannerOptions :: Prelude.Maybe ClientLoginBannerResponseOptions,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | Information about the client connection logging options for the Client
    -- VPN endpoint.
    connectionLogOptions :: Prelude.Maybe ConnectionLogResponseOptions,
    -- | The date and time the Client VPN endpoint was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Client VPN endpoint was deleted, if applicable.
    deletionTime :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The DNS name to be used by clients when connecting to the Client VPN
    -- endpoint.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | Information about the DNS servers to be used for DNS resolution.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the security groups for the target network.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The URL of the self-service portal.
    selfServicePortalUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the server certificate.
    serverCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum VPN session duration time in hours.
    --
    -- Valid values: @8 | 10 | 12 | 24@
    --
    -- Default value: @24@
    sessionTimeoutHours :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether split-tunnel is enabled in the Client VPN endpoint.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel Client VPN endpoint>
    -- in the /Client VPN Administrator Guide/.
    splitTunnel :: Prelude.Maybe Prelude.Bool,
    -- | The current state of the Client VPN endpoint.
    status :: Prelude.Maybe ClientVpnEndpointStatus,
    -- | Any tags assigned to the Client VPN endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | The transport protocol used by the Client VPN endpoint.
    transportProtocol :: Prelude.Maybe TransportProtocol,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The port number for the Client VPN endpoint.
    vpnPort :: Prelude.Maybe Prelude.Int,
    -- | The protocol used by the VPN session.
    vpnProtocol :: Prelude.Maybe VpnProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedTargetNetworks', 'clientVpnEndpoint_associatedTargetNetworks' - Information about the associated target networks. A target network is a
-- subnet in a VPC.
--
-- 'authenticationOptions', 'clientVpnEndpoint_authenticationOptions' - Information about the authentication method used by the Client VPN
-- endpoint.
--
-- 'clientCidrBlock', 'clientVpnEndpoint_clientCidrBlock' - The IPv4 address range, in CIDR notation, from which client IP addresses
-- are assigned.
--
-- 'clientConnectOptions', 'clientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'clientLoginBannerOptions', 'clientVpnEndpoint_clientLoginBannerOptions' - Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
--
-- 'clientVpnEndpointId', 'clientVpnEndpoint_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'connectionLogOptions', 'clientVpnEndpoint_connectionLogOptions' - Information about the client connection logging options for the Client
-- VPN endpoint.
--
-- 'creationTime', 'clientVpnEndpoint_creationTime' - The date and time the Client VPN endpoint was created.
--
-- 'deletionTime', 'clientVpnEndpoint_deletionTime' - The date and time the Client VPN endpoint was deleted, if applicable.
--
-- 'description', 'clientVpnEndpoint_description' - A brief description of the endpoint.
--
-- 'dnsName', 'clientVpnEndpoint_dnsName' - The DNS name to be used by clients when connecting to the Client VPN
-- endpoint.
--
-- 'dnsServers', 'clientVpnEndpoint_dnsServers' - Information about the DNS servers to be used for DNS resolution.
--
-- 'securityGroupIds', 'clientVpnEndpoint_securityGroupIds' - The IDs of the security groups for the target network.
--
-- 'selfServicePortalUrl', 'clientVpnEndpoint_selfServicePortalUrl' - The URL of the self-service portal.
--
-- 'serverCertificateArn', 'clientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate.
--
-- 'sessionTimeoutHours', 'clientVpnEndpoint_sessionTimeoutHours' - The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
--
-- 'splitTunnel', 'clientVpnEndpoint_splitTunnel' - Indicates whether split-tunnel is enabled in the Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
--
-- 'status', 'clientVpnEndpoint_status' - The current state of the Client VPN endpoint.
--
-- 'tags', 'clientVpnEndpoint_tags' - Any tags assigned to the Client VPN endpoint.
--
-- 'transportProtocol', 'clientVpnEndpoint_transportProtocol' - The transport protocol used by the Client VPN endpoint.
--
-- 'vpcId', 'clientVpnEndpoint_vpcId' - The ID of the VPC.
--
-- 'vpnPort', 'clientVpnEndpoint_vpnPort' - The port number for the Client VPN endpoint.
--
-- 'vpnProtocol', 'clientVpnEndpoint_vpnProtocol' - The protocol used by the VPN session.
newClientVpnEndpoint ::
  ClientVpnEndpoint
newClientVpnEndpoint =
  ClientVpnEndpoint'
    { associatedTargetNetworks =
        Prelude.Nothing,
      authenticationOptions = Prelude.Nothing,
      clientCidrBlock = Prelude.Nothing,
      clientConnectOptions = Prelude.Nothing,
      clientLoginBannerOptions = Prelude.Nothing,
      clientVpnEndpointId = Prelude.Nothing,
      connectionLogOptions = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      description = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      dnsServers = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      selfServicePortalUrl = Prelude.Nothing,
      serverCertificateArn = Prelude.Nothing,
      sessionTimeoutHours = Prelude.Nothing,
      splitTunnel = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      transportProtocol = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpnPort = Prelude.Nothing,
      vpnProtocol = Prelude.Nothing
    }

-- | Information about the associated target networks. A target network is a
-- subnet in a VPC.
clientVpnEndpoint_associatedTargetNetworks :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [AssociatedTargetNetwork])
clientVpnEndpoint_associatedTargetNetworks = Lens.lens (\ClientVpnEndpoint' {associatedTargetNetworks} -> associatedTargetNetworks) (\s@ClientVpnEndpoint' {} a -> s {associatedTargetNetworks = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Information about the authentication method used by the Client VPN
-- endpoint.
clientVpnEndpoint_authenticationOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [ClientVpnAuthentication])
clientVpnEndpoint_authenticationOptions = Lens.lens (\ClientVpnEndpoint' {authenticationOptions} -> authenticationOptions) (\s@ClientVpnEndpoint' {} a -> s {authenticationOptions = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 address range, in CIDR notation, from which client IP addresses
-- are assigned.
clientVpnEndpoint_clientCidrBlock :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_clientCidrBlock = Lens.lens (\ClientVpnEndpoint' {clientCidrBlock} -> clientCidrBlock) (\s@ClientVpnEndpoint' {} a -> s {clientCidrBlock = a} :: ClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
clientVpnEndpoint_clientConnectOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ClientConnectResponseOptions)
clientVpnEndpoint_clientConnectOptions = Lens.lens (\ClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@ClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: ClientVpnEndpoint)

-- | Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
clientVpnEndpoint_clientLoginBannerOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ClientLoginBannerResponseOptions)
clientVpnEndpoint_clientLoginBannerOptions = Lens.lens (\ClientVpnEndpoint' {clientLoginBannerOptions} -> clientLoginBannerOptions) (\s@ClientVpnEndpoint' {} a -> s {clientLoginBannerOptions = a} :: ClientVpnEndpoint)

-- | The ID of the Client VPN endpoint.
clientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_clientVpnEndpointId = Lens.lens (\ClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: ClientVpnEndpoint)

-- | Information about the client connection logging options for the Client
-- VPN endpoint.
clientVpnEndpoint_connectionLogOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ConnectionLogResponseOptions)
clientVpnEndpoint_connectionLogOptions = Lens.lens (\ClientVpnEndpoint' {connectionLogOptions} -> connectionLogOptions) (\s@ClientVpnEndpoint' {} a -> s {connectionLogOptions = a} :: ClientVpnEndpoint)

-- | The date and time the Client VPN endpoint was created.
clientVpnEndpoint_creationTime :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_creationTime = Lens.lens (\ClientVpnEndpoint' {creationTime} -> creationTime) (\s@ClientVpnEndpoint' {} a -> s {creationTime = a} :: ClientVpnEndpoint)

-- | The date and time the Client VPN endpoint was deleted, if applicable.
clientVpnEndpoint_deletionTime :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_deletionTime = Lens.lens (\ClientVpnEndpoint' {deletionTime} -> deletionTime) (\s@ClientVpnEndpoint' {} a -> s {deletionTime = a} :: ClientVpnEndpoint)

-- | A brief description of the endpoint.
clientVpnEndpoint_description :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_description = Lens.lens (\ClientVpnEndpoint' {description} -> description) (\s@ClientVpnEndpoint' {} a -> s {description = a} :: ClientVpnEndpoint)

-- | The DNS name to be used by clients when connecting to the Client VPN
-- endpoint.
clientVpnEndpoint_dnsName :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_dnsName = Lens.lens (\ClientVpnEndpoint' {dnsName} -> dnsName) (\s@ClientVpnEndpoint' {} a -> s {dnsName = a} :: ClientVpnEndpoint)

-- | Information about the DNS servers to be used for DNS resolution.
clientVpnEndpoint_dnsServers :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
clientVpnEndpoint_dnsServers = Lens.lens (\ClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@ClientVpnEndpoint' {} a -> s {dnsServers = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the security groups for the target network.
clientVpnEndpoint_securityGroupIds :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
clientVpnEndpoint_securityGroupIds = Lens.lens (\ClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@ClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the self-service portal.
clientVpnEndpoint_selfServicePortalUrl :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_selfServicePortalUrl = Lens.lens (\ClientVpnEndpoint' {selfServicePortalUrl} -> selfServicePortalUrl) (\s@ClientVpnEndpoint' {} a -> s {selfServicePortalUrl = a} :: ClientVpnEndpoint)

-- | The ARN of the server certificate.
clientVpnEndpoint_serverCertificateArn :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_serverCertificateArn = Lens.lens (\ClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@ClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: ClientVpnEndpoint)

-- | The maximum VPN session duration time in hours.
--
-- Valid values: @8 | 10 | 12 | 24@
--
-- Default value: @24@
clientVpnEndpoint_sessionTimeoutHours :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Int)
clientVpnEndpoint_sessionTimeoutHours = Lens.lens (\ClientVpnEndpoint' {sessionTimeoutHours} -> sessionTimeoutHours) (\s@ClientVpnEndpoint' {} a -> s {sessionTimeoutHours = a} :: ClientVpnEndpoint)

-- | Indicates whether split-tunnel is enabled in the Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel Client VPN endpoint>
-- in the /Client VPN Administrator Guide/.
clientVpnEndpoint_splitTunnel :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
clientVpnEndpoint_splitTunnel = Lens.lens (\ClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@ClientVpnEndpoint' {} a -> s {splitTunnel = a} :: ClientVpnEndpoint)

-- | The current state of the Client VPN endpoint.
clientVpnEndpoint_status :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ClientVpnEndpointStatus)
clientVpnEndpoint_status = Lens.lens (\ClientVpnEndpoint' {status} -> status) (\s@ClientVpnEndpoint' {} a -> s {status = a} :: ClientVpnEndpoint)

-- | Any tags assigned to the Client VPN endpoint.
clientVpnEndpoint_tags :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Tag])
clientVpnEndpoint_tags = Lens.lens (\ClientVpnEndpoint' {tags} -> tags) (\s@ClientVpnEndpoint' {} a -> s {tags = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The transport protocol used by the Client VPN endpoint.
clientVpnEndpoint_transportProtocol :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe TransportProtocol)
clientVpnEndpoint_transportProtocol = Lens.lens (\ClientVpnEndpoint' {transportProtocol} -> transportProtocol) (\s@ClientVpnEndpoint' {} a -> s {transportProtocol = a} :: ClientVpnEndpoint)

-- | The ID of the VPC.
clientVpnEndpoint_vpcId :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_vpcId = Lens.lens (\ClientVpnEndpoint' {vpcId} -> vpcId) (\s@ClientVpnEndpoint' {} a -> s {vpcId = a} :: ClientVpnEndpoint)

-- | The port number for the Client VPN endpoint.
clientVpnEndpoint_vpnPort :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Int)
clientVpnEndpoint_vpnPort = Lens.lens (\ClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@ClientVpnEndpoint' {} a -> s {vpnPort = a} :: ClientVpnEndpoint)

-- | The protocol used by the VPN session.
clientVpnEndpoint_vpnProtocol :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe VpnProtocol)
clientVpnEndpoint_vpnProtocol = Lens.lens (\ClientVpnEndpoint' {vpnProtocol} -> vpnProtocol) (\s@ClientVpnEndpoint' {} a -> s {vpnProtocol = a} :: ClientVpnEndpoint)

instance Data.FromXML ClientVpnEndpoint where
  parseXML x =
    ClientVpnEndpoint'
      Prelude.<$> ( x
                      Data..@? "associatedTargetNetwork"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "authenticationOptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "clientCidrBlock")
      Prelude.<*> (x Data..@? "clientConnectOptions")
      Prelude.<*> (x Data..@? "clientLoginBannerOptions")
      Prelude.<*> (x Data..@? "clientVpnEndpointId")
      Prelude.<*> (x Data..@? "connectionLogOptions")
      Prelude.<*> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "deletionTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "dnsName")
      Prelude.<*> ( x
                      Data..@? "dnsServer"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "securityGroupIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "selfServicePortalUrl")
      Prelude.<*> (x Data..@? "serverCertificateArn")
      Prelude.<*> (x Data..@? "sessionTimeoutHours")
      Prelude.<*> (x Data..@? "splitTunnel")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transportProtocol")
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@? "vpnPort")
      Prelude.<*> (x Data..@? "vpnProtocol")

instance Prelude.Hashable ClientVpnEndpoint where
  hashWithSalt _salt ClientVpnEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` associatedTargetNetworks
      `Prelude.hashWithSalt` authenticationOptions
      `Prelude.hashWithSalt` clientCidrBlock
      `Prelude.hashWithSalt` clientConnectOptions
      `Prelude.hashWithSalt` clientLoginBannerOptions
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` connectionLogOptions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deletionTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` dnsServers
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` selfServicePortalUrl
      `Prelude.hashWithSalt` serverCertificateArn
      `Prelude.hashWithSalt` sessionTimeoutHours
      `Prelude.hashWithSalt` splitTunnel
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transportProtocol
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpnPort
      `Prelude.hashWithSalt` vpnProtocol

instance Prelude.NFData ClientVpnEndpoint where
  rnf ClientVpnEndpoint' {..} =
    Prelude.rnf associatedTargetNetworks
      `Prelude.seq` Prelude.rnf authenticationOptions
      `Prelude.seq` Prelude.rnf clientCidrBlock
      `Prelude.seq` Prelude.rnf clientConnectOptions
      `Prelude.seq` Prelude.rnf clientLoginBannerOptions
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf connectionLogOptions
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf dnsServers
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf selfServicePortalUrl
      `Prelude.seq` Prelude.rnf serverCertificateArn
      `Prelude.seq` Prelude.rnf sessionTimeoutHours
      `Prelude.seq` Prelude.rnf splitTunnel
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transportProtocol
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpnPort
      `Prelude.seq` Prelude.rnf vpnProtocol
