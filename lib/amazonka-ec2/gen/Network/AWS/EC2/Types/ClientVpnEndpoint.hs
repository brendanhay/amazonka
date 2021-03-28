{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnEndpoint
  ( ClientVpnEndpoint (..)
  -- * Smart constructor
  , mkClientVpnEndpoint
  -- * Lenses
  , cveAssociatedTargetNetworks
  , cveAuthenticationOptions
  , cveClientCidrBlock
  , cveClientConnectOptions
  , cveClientVpnEndpointId
  , cveConnectionLogOptions
  , cveCreationTime
  , cveDeletionTime
  , cveDescription
  , cveDnsName
  , cveDnsServers
  , cveSecurityGroupIds
  , cveSelfServicePortalUrl
  , cveServerCertificateArn
  , cveSplitTunnel
  , cveStatus
  , cveTags
  , cveTransportProtocol
  , cveVpcId
  , cveVpnPort
  , cveVpnProtocol
  ) where

import qualified Network.AWS.EC2.Types.AssociatedTargetNetwork as Types
import qualified Network.AWS.EC2.Types.ClientConnectResponseOptions as Types
import qualified Network.AWS.EC2.Types.ClientVpnAuthentication as Types
import qualified Network.AWS.EC2.Types.ClientVpnEndpointStatus as Types
import qualified Network.AWS.EC2.Types.ConnectionLogResponseOptions as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransportProtocol as Types
import qualified Network.AWS.EC2.Types.VpcId as Types
import qualified Network.AWS.EC2.Types.VpnProtocol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Client VPN endpoint.
--
-- /See:/ 'mkClientVpnEndpoint' smart constructor.
data ClientVpnEndpoint = ClientVpnEndpoint'
  { associatedTargetNetworks :: Core.Maybe [Types.AssociatedTargetNetwork]
    -- ^ Information about the associated target networks. A target network is a subnet in a VPC.
  , authenticationOptions :: Core.Maybe [Types.ClientVpnAuthentication]
    -- ^ Information about the authentication method used by the Client VPN endpoint.
  , clientCidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
  , clientConnectOptions :: Core.Maybe Types.ClientConnectResponseOptions
    -- ^ The options for managing connection authorization for new client connections.
  , clientVpnEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the Client VPN endpoint.
  , connectionLogOptions :: Core.Maybe Types.ConnectionLogResponseOptions
    -- ^ Information about the client connection logging options for the Client VPN endpoint.
  , creationTime :: Core.Maybe Core.Text
    -- ^ The date and time the Client VPN endpoint was created.
  , deletionTime :: Core.Maybe Core.Text
    -- ^ The date and time the Client VPN endpoint was deleted, if applicable.
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the endpoint.
  , dnsName :: Core.Maybe Core.Text
    -- ^ The DNS name to be used by clients when connecting to the Client VPN endpoint.
  , dnsServers :: Core.Maybe [Core.Text]
    -- ^ Information about the DNS servers to be used for DNS resolution. 
  , securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups for the target network.
  , selfServicePortalUrl :: Core.Maybe Core.Text
    -- ^ The URL of the self-service portal.
  , serverCertificateArn :: Core.Maybe Core.Text
    -- ^ The ARN of the server certificate.
  , splitTunnel :: Core.Maybe Core.Bool
    -- ^ Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
  , status :: Core.Maybe Types.ClientVpnEndpointStatus
    -- ^ The current state of the Client VPN endpoint.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the Client VPN endpoint.
  , transportProtocol :: Core.Maybe Types.TransportProtocol
    -- ^ The transport protocol used by the Client VPN endpoint.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC.
  , vpnPort :: Core.Maybe Core.Int
    -- ^ The port number for the Client VPN endpoint.
  , vpnProtocol :: Core.Maybe Types.VpnProtocol
    -- ^ The protocol used by the VPN session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnEndpoint' value with any optional fields omitted.
mkClientVpnEndpoint
    :: ClientVpnEndpoint
mkClientVpnEndpoint
  = ClientVpnEndpoint'{associatedTargetNetworks = Core.Nothing,
                       authenticationOptions = Core.Nothing,
                       clientCidrBlock = Core.Nothing,
                       clientConnectOptions = Core.Nothing,
                       clientVpnEndpointId = Core.Nothing,
                       connectionLogOptions = Core.Nothing, creationTime = Core.Nothing,
                       deletionTime = Core.Nothing, description = Core.Nothing,
                       dnsName = Core.Nothing, dnsServers = Core.Nothing,
                       securityGroupIds = Core.Nothing,
                       selfServicePortalUrl = Core.Nothing,
                       serverCertificateArn = Core.Nothing, splitTunnel = Core.Nothing,
                       status = Core.Nothing, tags = Core.Nothing,
                       transportProtocol = Core.Nothing, vpcId = Core.Nothing,
                       vpnPort = Core.Nothing, vpnProtocol = Core.Nothing}

-- | Information about the associated target networks. A target network is a subnet in a VPC.
--
-- /Note:/ Consider using 'associatedTargetNetworks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveAssociatedTargetNetworks :: Lens.Lens' ClientVpnEndpoint (Core.Maybe [Types.AssociatedTargetNetwork])
cveAssociatedTargetNetworks = Lens.field @"associatedTargetNetworks"
{-# INLINEABLE cveAssociatedTargetNetworks #-}
{-# DEPRECATED associatedTargetNetworks "Use generic-lens or generic-optics with 'associatedTargetNetworks' instead"  #-}

-- | Information about the authentication method used by the Client VPN endpoint.
--
-- /Note:/ Consider using 'authenticationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveAuthenticationOptions :: Lens.Lens' ClientVpnEndpoint (Core.Maybe [Types.ClientVpnAuthentication])
cveAuthenticationOptions = Lens.field @"authenticationOptions"
{-# INLINEABLE cveAuthenticationOptions #-}
{-# DEPRECATED authenticationOptions "Use generic-lens or generic-optics with 'authenticationOptions' instead"  #-}

-- | The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
--
-- /Note:/ Consider using 'clientCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientCidrBlock :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveClientCidrBlock = Lens.field @"clientCidrBlock"
{-# INLINEABLE cveClientCidrBlock #-}
{-# DEPRECATED clientCidrBlock "Use generic-lens or generic-optics with 'clientCidrBlock' instead"  #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientConnectOptions :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.ClientConnectResponseOptions)
cveClientConnectOptions = Lens.field @"clientConnectOptions"
{-# INLINEABLE cveClientConnectOptions #-}
{-# DEPRECATED clientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead"  #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveClientVpnEndpointId :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE cveClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Information about the client connection logging options for the Client VPN endpoint.
--
-- /Note:/ Consider using 'connectionLogOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveConnectionLogOptions :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.ConnectionLogResponseOptions)
cveConnectionLogOptions = Lens.field @"connectionLogOptions"
{-# INLINEABLE cveConnectionLogOptions #-}
{-# DEPRECATED connectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead"  #-}

-- | The date and time the Client VPN endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveCreationTime :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveCreationTime = Lens.field @"creationTime"
{-# INLINEABLE cveCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time the Client VPN endpoint was deleted, if applicable.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDeletionTime :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveDeletionTime = Lens.field @"deletionTime"
{-# INLINEABLE cveDeletionTime #-}
{-# DEPRECATED deletionTime "Use generic-lens or generic-optics with 'deletionTime' instead"  #-}

-- | A brief description of the endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDescription :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveDescription = Lens.field @"description"
{-# INLINEABLE cveDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The DNS name to be used by clients when connecting to the Client VPN endpoint.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDnsName :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveDnsName = Lens.field @"dnsName"
{-# INLINEABLE cveDnsName #-}
{-# DEPRECATED dnsName "Use generic-lens or generic-optics with 'dnsName' instead"  #-}

-- | Information about the DNS servers to be used for DNS resolution. 
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveDnsServers :: Lens.Lens' ClientVpnEndpoint (Core.Maybe [Core.Text])
cveDnsServers = Lens.field @"dnsServers"
{-# INLINEABLE cveDnsServers #-}
{-# DEPRECATED dnsServers "Use generic-lens or generic-optics with 'dnsServers' instead"  #-}

-- | The IDs of the security groups for the target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSecurityGroupIds :: Lens.Lens' ClientVpnEndpoint (Core.Maybe [Types.SecurityGroupId])
cveSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE cveSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The URL of the self-service portal.
--
-- /Note:/ Consider using 'selfServicePortalUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSelfServicePortalUrl :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveSelfServicePortalUrl = Lens.field @"selfServicePortalUrl"
{-# INLINEABLE cveSelfServicePortalUrl #-}
{-# DEPRECATED selfServicePortalUrl "Use generic-lens or generic-optics with 'selfServicePortalUrl' instead"  #-}

-- | The ARN of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveServerCertificateArn :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Text)
cveServerCertificateArn = Lens.field @"serverCertificateArn"
{-# INLINEABLE cveServerCertificateArn #-}
{-# DEPRECATED serverCertificateArn "Use generic-lens or generic-optics with 'serverCertificateArn' instead"  #-}

-- | Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveSplitTunnel :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Bool)
cveSplitTunnel = Lens.field @"splitTunnel"
{-# INLINEABLE cveSplitTunnel #-}
{-# DEPRECATED splitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead"  #-}

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveStatus :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.ClientVpnEndpointStatus)
cveStatus = Lens.field @"status"
{-# INLINEABLE cveStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Any tags assigned to the Client VPN endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveTags :: Lens.Lens' ClientVpnEndpoint (Core.Maybe [Types.Tag])
cveTags = Lens.field @"tags"
{-# INLINEABLE cveTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The transport protocol used by the Client VPN endpoint.
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveTransportProtocol :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.TransportProtocol)
cveTransportProtocol = Lens.field @"transportProtocol"
{-# INLINEABLE cveTransportProtocol #-}
{-# DEPRECATED transportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVpcId :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.VpcId)
cveVpcId = Lens.field @"vpcId"
{-# INLINEABLE cveVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The port number for the Client VPN endpoint.
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVpnPort :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Core.Int)
cveVpnPort = Lens.field @"vpnPort"
{-# INLINEABLE cveVpnPort #-}
{-# DEPRECATED vpnPort "Use generic-lens or generic-optics with 'vpnPort' instead"  #-}

-- | The protocol used by the VPN session.
--
-- /Note:/ Consider using 'vpnProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveVpnProtocol :: Lens.Lens' ClientVpnEndpoint (Core.Maybe Types.VpnProtocol)
cveVpnProtocol = Lens.field @"vpnProtocol"
{-# INLINEABLE cveVpnProtocol #-}
{-# DEPRECATED vpnProtocol "Use generic-lens or generic-optics with 'vpnProtocol' instead"  #-}

instance Core.FromXML ClientVpnEndpoint where
        parseXML x
          = ClientVpnEndpoint' Core.<$>
              (x Core..@? "associatedTargetNetwork" Core..<@>
                 Core.parseXMLList "item")
                Core.<*>
                x Core..@? "authenticationOptions" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "clientCidrBlock"
                Core.<*> x Core..@? "clientConnectOptions"
                Core.<*> x Core..@? "clientVpnEndpointId"
                Core.<*> x Core..@? "connectionLogOptions"
                Core.<*> x Core..@? "creationTime"
                Core.<*> x Core..@? "deletionTime"
                Core.<*> x Core..@? "description"
                Core.<*> x Core..@? "dnsName"
                Core.<*> x Core..@? "dnsServer" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "securityGroupIdSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "selfServicePortalUrl"
                Core.<*> x Core..@? "serverCertificateArn"
                Core.<*> x Core..@? "splitTunnel"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transportProtocol"
                Core.<*> x Core..@? "vpcId"
                Core.<*> x Core..@? "vpnPort"
                Core.<*> x Core..@? "vpnProtocol"
