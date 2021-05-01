{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnEndpoint where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientVpnAuthentication
import Network.AWS.EC2.Types.ClientVpnEndpointStatus
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.VpnProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Client VPN endpoint.
--
-- /See:/ 'newClientVpnEndpoint' smart constructor.
data ClientVpnEndpoint = ClientVpnEndpoint'
  { -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups for the target network.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The IPv4 address range, in CIDR notation, from which client IP addresses
    -- are assigned.
    clientCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Client VPN endpoint.
    status :: Prelude.Maybe ClientVpnEndpointStatus,
    -- | The date and time the Client VPN endpoint was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | Information about the associated target networks. A target network is a
    -- subnet in a VPC.
    associatedTargetNetworks :: Prelude.Maybe [AssociatedTargetNetwork],
    -- | The date and time the Client VPN endpoint was deleted, if applicable.
    deletionTime :: Prelude.Maybe Prelude.Text,
    -- | The URL of the self-service portal.
    selfServicePortalUrl :: Prelude.Maybe Prelude.Text,
    -- | Information about the authentication method used by the Client VPN
    -- endpoint.
    authenticationOptions :: Prelude.Maybe [ClientVpnAuthentication],
    -- | The ARN of the server certificate.
    serverCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The transport protocol used by the Client VPN endpoint.
    transportProtocol :: Prelude.Maybe TransportProtocol,
    -- | Information about the client connection logging options for the Client
    -- VPN endpoint.
    connectionLogOptions :: Prelude.Maybe ConnectionLogResponseOptions,
    -- | The options for managing connection authorization for new client
    -- connections.
    clientConnectOptions :: Prelude.Maybe ClientConnectResponseOptions,
    -- | Information about the DNS servers to be used for DNS resolution.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | Any tags assigned to the Client VPN endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | The port number for the Client VPN endpoint.
    vpnPort :: Prelude.Maybe Prelude.Int,
    -- | The DNS name to be used by clients when connecting to the Client VPN
    -- endpoint.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The protocol used by the VPN session.
    vpnProtocol :: Prelude.Maybe VpnProtocol,
    -- | A brief description of the endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether split-tunnel is enabled in the AWS Client VPN
    -- endpoint.
    --
    -- For information about split-tunnel VPN endpoints, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
    -- in the /AWS Client VPN Administrator Guide/.
    splitTunnel :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'clientVpnEndpoint_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'securityGroupIds', 'clientVpnEndpoint_securityGroupIds' - The IDs of the security groups for the target network.
--
-- 'clientCidrBlock', 'clientVpnEndpoint_clientCidrBlock' - The IPv4 address range, in CIDR notation, from which client IP addresses
-- are assigned.
--
-- 'status', 'clientVpnEndpoint_status' - The current state of the Client VPN endpoint.
--
-- 'creationTime', 'clientVpnEndpoint_creationTime' - The date and time the Client VPN endpoint was created.
--
-- 'associatedTargetNetworks', 'clientVpnEndpoint_associatedTargetNetworks' - Information about the associated target networks. A target network is a
-- subnet in a VPC.
--
-- 'deletionTime', 'clientVpnEndpoint_deletionTime' - The date and time the Client VPN endpoint was deleted, if applicable.
--
-- 'selfServicePortalUrl', 'clientVpnEndpoint_selfServicePortalUrl' - The URL of the self-service portal.
--
-- 'authenticationOptions', 'clientVpnEndpoint_authenticationOptions' - Information about the authentication method used by the Client VPN
-- endpoint.
--
-- 'serverCertificateArn', 'clientVpnEndpoint_serverCertificateArn' - The ARN of the server certificate.
--
-- 'transportProtocol', 'clientVpnEndpoint_transportProtocol' - The transport protocol used by the Client VPN endpoint.
--
-- 'connectionLogOptions', 'clientVpnEndpoint_connectionLogOptions' - Information about the client connection logging options for the Client
-- VPN endpoint.
--
-- 'clientConnectOptions', 'clientVpnEndpoint_clientConnectOptions' - The options for managing connection authorization for new client
-- connections.
--
-- 'dnsServers', 'clientVpnEndpoint_dnsServers' - Information about the DNS servers to be used for DNS resolution.
--
-- 'tags', 'clientVpnEndpoint_tags' - Any tags assigned to the Client VPN endpoint.
--
-- 'vpnPort', 'clientVpnEndpoint_vpnPort' - The port number for the Client VPN endpoint.
--
-- 'dnsName', 'clientVpnEndpoint_dnsName' - The DNS name to be used by clients when connecting to the Client VPN
-- endpoint.
--
-- 'vpnProtocol', 'clientVpnEndpoint_vpnProtocol' - The protocol used by the VPN session.
--
-- 'description', 'clientVpnEndpoint_description' - A brief description of the endpoint.
--
-- 'vpcId', 'clientVpnEndpoint_vpcId' - The ID of the VPC.
--
-- 'splitTunnel', 'clientVpnEndpoint_splitTunnel' - Indicates whether split-tunnel is enabled in the AWS Client VPN
-- endpoint.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
newClientVpnEndpoint ::
  ClientVpnEndpoint
newClientVpnEndpoint =
  ClientVpnEndpoint'
    { clientVpnEndpointId =
        Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      clientCidrBlock = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      associatedTargetNetworks = Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      selfServicePortalUrl = Prelude.Nothing,
      authenticationOptions = Prelude.Nothing,
      serverCertificateArn = Prelude.Nothing,
      transportProtocol = Prelude.Nothing,
      connectionLogOptions = Prelude.Nothing,
      clientConnectOptions = Prelude.Nothing,
      dnsServers = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpnPort = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      vpnProtocol = Prelude.Nothing,
      description = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      splitTunnel = Prelude.Nothing
    }

-- | The ID of the Client VPN endpoint.
clientVpnEndpoint_clientVpnEndpointId :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_clientVpnEndpointId = Lens.lens (\ClientVpnEndpoint' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnEndpoint' {} a -> s {clientVpnEndpointId = a} :: ClientVpnEndpoint)

-- | The IDs of the security groups for the target network.
clientVpnEndpoint_securityGroupIds :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
clientVpnEndpoint_securityGroupIds = Lens.lens (\ClientVpnEndpoint' {securityGroupIds} -> securityGroupIds) (\s@ClientVpnEndpoint' {} a -> s {securityGroupIds = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv4 address range, in CIDR notation, from which client IP addresses
-- are assigned.
clientVpnEndpoint_clientCidrBlock :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_clientCidrBlock = Lens.lens (\ClientVpnEndpoint' {clientCidrBlock} -> clientCidrBlock) (\s@ClientVpnEndpoint' {} a -> s {clientCidrBlock = a} :: ClientVpnEndpoint)

-- | The current state of the Client VPN endpoint.
clientVpnEndpoint_status :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ClientVpnEndpointStatus)
clientVpnEndpoint_status = Lens.lens (\ClientVpnEndpoint' {status} -> status) (\s@ClientVpnEndpoint' {} a -> s {status = a} :: ClientVpnEndpoint)

-- | The date and time the Client VPN endpoint was created.
clientVpnEndpoint_creationTime :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_creationTime = Lens.lens (\ClientVpnEndpoint' {creationTime} -> creationTime) (\s@ClientVpnEndpoint' {} a -> s {creationTime = a} :: ClientVpnEndpoint)

-- | Information about the associated target networks. A target network is a
-- subnet in a VPC.
clientVpnEndpoint_associatedTargetNetworks :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [AssociatedTargetNetwork])
clientVpnEndpoint_associatedTargetNetworks = Lens.lens (\ClientVpnEndpoint' {associatedTargetNetworks} -> associatedTargetNetworks) (\s@ClientVpnEndpoint' {} a -> s {associatedTargetNetworks = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time the Client VPN endpoint was deleted, if applicable.
clientVpnEndpoint_deletionTime :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_deletionTime = Lens.lens (\ClientVpnEndpoint' {deletionTime} -> deletionTime) (\s@ClientVpnEndpoint' {} a -> s {deletionTime = a} :: ClientVpnEndpoint)

-- | The URL of the self-service portal.
clientVpnEndpoint_selfServicePortalUrl :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_selfServicePortalUrl = Lens.lens (\ClientVpnEndpoint' {selfServicePortalUrl} -> selfServicePortalUrl) (\s@ClientVpnEndpoint' {} a -> s {selfServicePortalUrl = a} :: ClientVpnEndpoint)

-- | Information about the authentication method used by the Client VPN
-- endpoint.
clientVpnEndpoint_authenticationOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [ClientVpnAuthentication])
clientVpnEndpoint_authenticationOptions = Lens.lens (\ClientVpnEndpoint' {authenticationOptions} -> authenticationOptions) (\s@ClientVpnEndpoint' {} a -> s {authenticationOptions = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the server certificate.
clientVpnEndpoint_serverCertificateArn :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_serverCertificateArn = Lens.lens (\ClientVpnEndpoint' {serverCertificateArn} -> serverCertificateArn) (\s@ClientVpnEndpoint' {} a -> s {serverCertificateArn = a} :: ClientVpnEndpoint)

-- | The transport protocol used by the Client VPN endpoint.
clientVpnEndpoint_transportProtocol :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe TransportProtocol)
clientVpnEndpoint_transportProtocol = Lens.lens (\ClientVpnEndpoint' {transportProtocol} -> transportProtocol) (\s@ClientVpnEndpoint' {} a -> s {transportProtocol = a} :: ClientVpnEndpoint)

-- | Information about the client connection logging options for the Client
-- VPN endpoint.
clientVpnEndpoint_connectionLogOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ConnectionLogResponseOptions)
clientVpnEndpoint_connectionLogOptions = Lens.lens (\ClientVpnEndpoint' {connectionLogOptions} -> connectionLogOptions) (\s@ClientVpnEndpoint' {} a -> s {connectionLogOptions = a} :: ClientVpnEndpoint)

-- | The options for managing connection authorization for new client
-- connections.
clientVpnEndpoint_clientConnectOptions :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe ClientConnectResponseOptions)
clientVpnEndpoint_clientConnectOptions = Lens.lens (\ClientVpnEndpoint' {clientConnectOptions} -> clientConnectOptions) (\s@ClientVpnEndpoint' {} a -> s {clientConnectOptions = a} :: ClientVpnEndpoint)

-- | Information about the DNS servers to be used for DNS resolution.
clientVpnEndpoint_dnsServers :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Prelude.Text])
clientVpnEndpoint_dnsServers = Lens.lens (\ClientVpnEndpoint' {dnsServers} -> dnsServers) (\s@ClientVpnEndpoint' {} a -> s {dnsServers = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags assigned to the Client VPN endpoint.
clientVpnEndpoint_tags :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe [Tag])
clientVpnEndpoint_tags = Lens.lens (\ClientVpnEndpoint' {tags} -> tags) (\s@ClientVpnEndpoint' {} a -> s {tags = a} :: ClientVpnEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The port number for the Client VPN endpoint.
clientVpnEndpoint_vpnPort :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Int)
clientVpnEndpoint_vpnPort = Lens.lens (\ClientVpnEndpoint' {vpnPort} -> vpnPort) (\s@ClientVpnEndpoint' {} a -> s {vpnPort = a} :: ClientVpnEndpoint)

-- | The DNS name to be used by clients when connecting to the Client VPN
-- endpoint.
clientVpnEndpoint_dnsName :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_dnsName = Lens.lens (\ClientVpnEndpoint' {dnsName} -> dnsName) (\s@ClientVpnEndpoint' {} a -> s {dnsName = a} :: ClientVpnEndpoint)

-- | The protocol used by the VPN session.
clientVpnEndpoint_vpnProtocol :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe VpnProtocol)
clientVpnEndpoint_vpnProtocol = Lens.lens (\ClientVpnEndpoint' {vpnProtocol} -> vpnProtocol) (\s@ClientVpnEndpoint' {} a -> s {vpnProtocol = a} :: ClientVpnEndpoint)

-- | A brief description of the endpoint.
clientVpnEndpoint_description :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_description = Lens.lens (\ClientVpnEndpoint' {description} -> description) (\s@ClientVpnEndpoint' {} a -> s {description = a} :: ClientVpnEndpoint)

-- | The ID of the VPC.
clientVpnEndpoint_vpcId :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Text)
clientVpnEndpoint_vpcId = Lens.lens (\ClientVpnEndpoint' {vpcId} -> vpcId) (\s@ClientVpnEndpoint' {} a -> s {vpcId = a} :: ClientVpnEndpoint)

-- | Indicates whether split-tunnel is enabled in the AWS Client VPN
-- endpoint.
--
-- For information about split-tunnel VPN endpoints, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint>
-- in the /AWS Client VPN Administrator Guide/.
clientVpnEndpoint_splitTunnel :: Lens.Lens' ClientVpnEndpoint (Prelude.Maybe Prelude.Bool)
clientVpnEndpoint_splitTunnel = Lens.lens (\ClientVpnEndpoint' {splitTunnel} -> splitTunnel) (\s@ClientVpnEndpoint' {} a -> s {splitTunnel = a} :: ClientVpnEndpoint)

instance Prelude.FromXML ClientVpnEndpoint where
  parseXML x =
    ClientVpnEndpoint'
      Prelude.<$> (x Prelude..@? "clientVpnEndpointId")
      Prelude.<*> ( x Prelude..@? "securityGroupIdSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "clientCidrBlock")
      Prelude.<*> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> ( x Prelude..@? "associatedTargetNetwork"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "deletionTime")
      Prelude.<*> (x Prelude..@? "selfServicePortalUrl")
      Prelude.<*> ( x Prelude..@? "authenticationOptions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "serverCertificateArn")
      Prelude.<*> (x Prelude..@? "transportProtocol")
      Prelude.<*> (x Prelude..@? "connectionLogOptions")
      Prelude.<*> (x Prelude..@? "clientConnectOptions")
      Prelude.<*> ( x Prelude..@? "dnsServer" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpnPort")
      Prelude.<*> (x Prelude..@? "dnsName")
      Prelude.<*> (x Prelude..@? "vpnProtocol")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "vpcId")
      Prelude.<*> (x Prelude..@? "splitTunnel")

instance Prelude.Hashable ClientVpnEndpoint

instance Prelude.NFData ClientVpnEndpoint
