{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpoint
  ( ClientVPNEndpoint (..),

    -- * Smart constructor
    mkClientVPNEndpoint,

    -- * Lenses
    cveCreationTime,
    cveStatus,
    cveAssociatedTargetNetworks,
    cveSecurityGroupIds,
    cveConnectionLogOptions,
    cveSplitTunnel,
    cveTransportProtocol,
    cveVPCId,
    cveVPNPort,
    cveDeletionTime,
    cveClientCidrBlock,
    cveDNSServers,
    cveClientVPNEndpointId,
    cveClientConnectOptions,
    cveServerCertificateARN,
    cveAuthenticationOptions,
    cveSelfServicePortalURL,
    cveDescription,
    cveDNSName,
    cveVPNProtocol,
    cveTags,
  )
where

import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientVPNAuthentication
import Network.AWS.EC2.Types.ClientVPNEndpointStatus
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.VPNProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Client VPN endpoint.
--
-- /See:/ 'mkClientVPNEndpoint' smart constructor.
data ClientVPNEndpoint = ClientVPNEndpoint'
  { creationTime ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe ClientVPNEndpointStatus,
    associatedTargetNetworks ::
      Lude.Maybe [AssociatedTargetNetwork],
    securityGroupIds :: Lude.Maybe [Lude.Text],
    connectionLogOptions ::
      Lude.Maybe ConnectionLogResponseOptions,
    splitTunnel :: Lude.Maybe Lude.Bool,
    transportProtocol :: Lude.Maybe TransportProtocol,
    vpcId :: Lude.Maybe Lude.Text,
    vpnPort :: Lude.Maybe Lude.Int,
    deletionTime :: Lude.Maybe Lude.Text,
    clientCidrBlock :: Lude.Maybe Lude.Text,
    dnsServers :: Lude.Maybe [Lude.Text],
    clientVPNEndpointId :: Lude.Maybe Lude.Text,
    clientConnectOptions ::
      Lude.Maybe ClientConnectResponseOptions,
    serverCertificateARN :: Lude.Maybe Lude.Text,
    authenticationOptions ::
      Lude.Maybe [ClientVPNAuthentication],
    selfServicePortalURL :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text,
    vpnProtocol :: Lude.Maybe VPNProtocol,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNEndpoint' with the minimum fields required to make a request.
--
-- * 'associatedTargetNetworks' - Information about the associated target networks. A target network is a subnet in a VPC.
-- * 'authenticationOptions' - Information about the authentication method used by the Client VPN endpoint.
-- * 'clientCidrBlock' - The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
-- * 'clientConnectOptions' - The options for managing connection authorization for new client connections.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'connectionLogOptions' - Information about the client connection logging options for the Client VPN endpoint.
-- * 'creationTime' - The date and time the Client VPN endpoint was created.
-- * 'deletionTime' - The date and time the Client VPN endpoint was deleted, if applicable.
-- * 'description' - A brief description of the endpoint.
-- * 'dnsName' - The DNS name to be used by clients when connecting to the Client VPN endpoint.
-- * 'dnsServers' - Information about the DNS servers to be used for DNS resolution.
-- * 'securityGroupIds' - The IDs of the security groups for the target network.
-- * 'selfServicePortalURL' - The URL of the self-service portal.
-- * 'serverCertificateARN' - The ARN of the server certificate.
-- * 'splitTunnel' - Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
-- * 'status' - The current state of the Client VPN endpoint.
-- * 'tags' - Any tags assigned to the Client VPN endpoint.
-- * 'transportProtocol' - The transport protocol used by the Client VPN endpoint.
-- * 'vpcId' - The ID of the VPC.
-- * 'vpnPort' - The port number for the Client VPN endpoint.
-- * 'vpnProtocol' - The protocol used by the VPN session.
mkClientVPNEndpoint ::
  ClientVPNEndpoint
mkClientVPNEndpoint =
  ClientVPNEndpoint'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      associatedTargetNetworks = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      connectionLogOptions = Lude.Nothing,
      splitTunnel = Lude.Nothing,
      transportProtocol = Lude.Nothing,
      vpcId = Lude.Nothing,
      vpnPort = Lude.Nothing,
      deletionTime = Lude.Nothing,
      clientCidrBlock = Lude.Nothing,
      dnsServers = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      clientConnectOptions = Lude.Nothing,
      serverCertificateARN = Lude.Nothing,
      authenticationOptions = Lude.Nothing,
      selfServicePortalURL = Lude.Nothing,
      description = Lude.Nothing,
      dnsName = Lude.Nothing,
      vpnProtocol = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The date and time the Client VPN endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveCreationTime :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveCreationTime = Lens.lens (creationTime :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveStatus :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe ClientVPNEndpointStatus)
cveStatus = Lens.lens (status :: ClientVPNEndpoint -> Lude.Maybe ClientVPNEndpointStatus) (\s a -> s {status = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the associated target networks. A target network is a subnet in a VPC.
--
-- /Note:/ Consider using 'associatedTargetNetworks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveAssociatedTargetNetworks :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe [AssociatedTargetNetwork])
cveAssociatedTargetNetworks = Lens.lens (associatedTargetNetworks :: ClientVPNEndpoint -> Lude.Maybe [AssociatedTargetNetwork]) (\s a -> s {associatedTargetNetworks = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveAssociatedTargetNetworks "Use generic-lens or generic-optics with 'associatedTargetNetworks' instead." #-}

-- | The IDs of the security groups for the target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSecurityGroupIds :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe [Lude.Text])
cveSecurityGroupIds = Lens.lens (securityGroupIds :: ClientVPNEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Information about the client connection logging options for the Client VPN endpoint.
--
-- /Note:/ Consider using 'connectionLogOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveConnectionLogOptions :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe ConnectionLogResponseOptions)
cveConnectionLogOptions = Lens.lens (connectionLogOptions :: ClientVPNEndpoint -> Lude.Maybe ConnectionLogResponseOptions) (\s a -> s {connectionLogOptions = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveConnectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead." #-}

-- | Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSplitTunnel :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Bool)
cveSplitTunnel = Lens.lens (splitTunnel :: ClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {splitTunnel = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveSplitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead." #-}

-- | The transport protocol used by the Client VPN endpoint.
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveTransportProtocol :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe TransportProtocol)
cveTransportProtocol = Lens.lens (transportProtocol :: ClientVPNEndpoint -> Lude.Maybe TransportProtocol) (\s a -> s {transportProtocol = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveTransportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVPCId :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveVPCId = Lens.lens (vpcId :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The port number for the Client VPN endpoint.
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVPNPort :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Int)
cveVPNPort = Lens.lens (vpnPort :: ClientVPNEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {vpnPort = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveVPNPort "Use generic-lens or generic-optics with 'vpnPort' instead." #-}

-- | The date and time the Client VPN endpoint was deleted, if applicable.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDeletionTime :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveDeletionTime = Lens.lens (deletionTime :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {deletionTime = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveDeletionTime "Use generic-lens or generic-optics with 'deletionTime' instead." #-}

-- | The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
--
-- /Note:/ Consider using 'clientCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientCidrBlock :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveClientCidrBlock = Lens.lens (clientCidrBlock :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {clientCidrBlock = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveClientCidrBlock "Use generic-lens or generic-optics with 'clientCidrBlock' instead." #-}

-- | Information about the DNS servers to be used for DNS resolution.
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDNSServers :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe [Lude.Text])
cveDNSServers = Lens.lens (dnsServers :: ClientVPNEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsServers = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveDNSServers "Use generic-lens or generic-optics with 'dnsServers' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientVPNEndpointId :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientConnectOptions :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe ClientConnectResponseOptions)
cveClientConnectOptions = Lens.lens (clientConnectOptions :: ClientVPNEndpoint -> Lude.Maybe ClientConnectResponseOptions) (\s a -> s {clientConnectOptions = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveClientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead." #-}

-- | The ARN of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveServerCertificateARN :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveServerCertificateARN = Lens.lens (serverCertificateARN :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serverCertificateARN = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveServerCertificateARN "Use generic-lens or generic-optics with 'serverCertificateARN' instead." #-}

-- | Information about the authentication method used by the Client VPN endpoint.
--
-- /Note:/ Consider using 'authenticationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveAuthenticationOptions :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe [ClientVPNAuthentication])
cveAuthenticationOptions = Lens.lens (authenticationOptions :: ClientVPNEndpoint -> Lude.Maybe [ClientVPNAuthentication]) (\s a -> s {authenticationOptions = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveAuthenticationOptions "Use generic-lens or generic-optics with 'authenticationOptions' instead." #-}

-- | The URL of the self-service portal.
--
-- /Note:/ Consider using 'selfServicePortalURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSelfServicePortalURL :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveSelfServicePortalURL = Lens.lens (selfServicePortalURL :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {selfServicePortalURL = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveSelfServicePortalURL "Use generic-lens or generic-optics with 'selfServicePortalURL' instead." #-}

-- | A brief description of the endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDescription :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveDescription = Lens.lens (description :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The DNS name to be used by clients when connecting to the Client VPN endpoint.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDNSName :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe Lude.Text)
cveDNSName = Lens.lens (dnsName :: ClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The protocol used by the VPN session.
--
-- /Note:/ Consider using 'vpnProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVPNProtocol :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe VPNProtocol)
cveVPNProtocol = Lens.lens (vpnProtocol :: ClientVPNEndpoint -> Lude.Maybe VPNProtocol) (\s a -> s {vpnProtocol = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveVPNProtocol "Use generic-lens or generic-optics with 'vpnProtocol' instead." #-}

-- | Any tags assigned to the Client VPN endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveTags :: Lens.Lens' ClientVPNEndpoint (Lude.Maybe [Tag])
cveTags = Lens.lens (tags :: ClientVPNEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ClientVPNEndpoint)
{-# DEPRECATED cveTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ClientVPNEndpoint where
  parseXML x =
    ClientVPNEndpoint'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> ( x Lude..@? "associatedTargetNetwork" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "securityGroupIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "connectionLogOptions")
      Lude.<*> (x Lude..@? "splitTunnel")
      Lude.<*> (x Lude..@? "transportProtocol")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "vpnPort")
      Lude.<*> (x Lude..@? "deletionTime")
      Lude.<*> (x Lude..@? "clientCidrBlock")
      Lude.<*> ( x Lude..@? "dnsServer" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "clientVpnEndpointId")
      Lude.<*> (x Lude..@? "clientConnectOptions")
      Lude.<*> (x Lude..@? "serverCertificateArn")
      Lude.<*> ( x Lude..@? "authenticationOptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "selfServicePortalUrl")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "dnsName")
      Lude.<*> (x Lude..@? "vpnProtocol")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
