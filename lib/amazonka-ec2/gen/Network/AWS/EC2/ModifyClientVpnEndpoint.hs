{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyClientVpnEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Client VPN endpoint. Modifying the DNS server resets existing client connections.
module Network.AWS.EC2.ModifyClientVpnEndpoint
    (
    -- * Creating a request
      ModifyClientVpnEndpoint (..)
    , mkModifyClientVpnEndpoint
    -- ** Request lenses
    , mcveClientVpnEndpointId
    , mcveClientConnectOptions
    , mcveConnectionLogOptions
    , mcveDescription
    , mcveDnsServers
    , mcveDryRun
    , mcveSecurityGroupIds
    , mcveSelfServicePortal
    , mcveServerCertificateArn
    , mcveSplitTunnel
    , mcveVpcId
    , mcveVpnPort

    -- * Destructuring the response
    , ModifyClientVpnEndpointResponse (..)
    , mkModifyClientVpnEndpointResponse
    -- ** Response lenses
    , mcverrsReturn
    , mcverrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClientVpnEndpoint' smart constructor.
data ModifyClientVpnEndpoint = ModifyClientVpnEndpoint'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint to modify.
  , clientConnectOptions :: Core.Maybe Types.ClientConnectOptions
    -- ^ The options for managing connection authorization for new client connections.
  , connectionLogOptions :: Core.Maybe Types.ConnectionLogOptions
    -- ^ Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:
--
--     * Client connection requests
--
--
--     * Client connection results (successful and unsuccessful)
--
--
--     * Reasons for unsuccessful client connection requests
--
--
--     * Client connection termination time
--
--
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the Client VPN endpoint.
  , dnsServers :: Core.Maybe Types.DnsServersOptionsModifyStructure
    -- ^ Information about the DNS servers to be used by Client VPN connections. A Client VPN endpoint can have up to two DNS servers.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of one or more security groups to apply to the target network.
  , selfServicePortal :: Core.Maybe Types.SelfServicePortal
    -- ^ Specify whether to enable the self-service portal for the Client VPN endpoint.
  , serverCertificateArn :: Core.Maybe Core.Text
    -- ^ The ARN of the server certificate to be used. The server certificate must be provisioned in AWS Certificate Manager (ACM).
  , splitTunnel :: Core.Maybe Core.Bool
    -- ^ Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC to associate with the Client VPN endpoint.
  , vpnPort :: Core.Maybe Core.Int
    -- ^ The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@ 
-- Default Value: @443@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClientVpnEndpoint' value with any optional fields omitted.
mkModifyClientVpnEndpoint
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> ModifyClientVpnEndpoint
mkModifyClientVpnEndpoint clientVpnEndpointId
  = ModifyClientVpnEndpoint'{clientVpnEndpointId,
                             clientConnectOptions = Core.Nothing,
                             connectionLogOptions = Core.Nothing, description = Core.Nothing,
                             dnsServers = Core.Nothing, dryRun = Core.Nothing,
                             securityGroupIds = Core.Nothing, selfServicePortal = Core.Nothing,
                             serverCertificateArn = Core.Nothing, splitTunnel = Core.Nothing,
                             vpcId = Core.Nothing, vpnPort = Core.Nothing}

-- | The ID of the Client VPN endpoint to modify.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveClientVpnEndpointId :: Lens.Lens' ModifyClientVpnEndpoint Types.ClientVpnEndpointId
mcveClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE mcveClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveClientConnectOptions :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Types.ClientConnectOptions)
mcveClientConnectOptions = Lens.field @"clientConnectOptions"
{-# INLINEABLE mcveClientConnectOptions #-}
{-# DEPRECATED clientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead"  #-}

-- | Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:
--
--     * Client connection requests
--
--
--     * Client connection results (successful and unsuccessful)
--
--
--     * Reasons for unsuccessful client connection requests
--
--
--     * Client connection termination time
--
--
--
-- /Note:/ Consider using 'connectionLogOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveConnectionLogOptions :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Types.ConnectionLogOptions)
mcveConnectionLogOptions = Lens.field @"connectionLogOptions"
{-# INLINEABLE mcveConnectionLogOptions #-}
{-# DEPRECATED connectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead"  #-}

-- | A brief description of the Client VPN endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDescription :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Text)
mcveDescription = Lens.field @"description"
{-# INLINEABLE mcveDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the DNS servers to be used by Client VPN connections. A Client VPN endpoint can have up to two DNS servers.
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDnsServers :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Types.DnsServersOptionsModifyStructure)
mcveDnsServers = Lens.field @"dnsServers"
{-# INLINEABLE mcveDnsServers #-}
{-# DEPRECATED dnsServers "Use generic-lens or generic-optics with 'dnsServers' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDryRun :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Bool)
mcveDryRun = Lens.field @"dryRun"
{-# INLINEABLE mcveDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IDs of one or more security groups to apply to the target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSecurityGroupIds :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe [Types.SecurityGroupId])
mcveSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE mcveSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- /Note:/ Consider using 'selfServicePortal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSelfServicePortal :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Types.SelfServicePortal)
mcveSelfServicePortal = Lens.field @"selfServicePortal"
{-# INLINEABLE mcveSelfServicePortal #-}
{-# DEPRECATED selfServicePortal "Use generic-lens or generic-optics with 'selfServicePortal' instead"  #-}

-- | The ARN of the server certificate to be used. The server certificate must be provisioned in AWS Certificate Manager (ACM).
--
-- /Note:/ Consider using 'serverCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveServerCertificateArn :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Text)
mcveServerCertificateArn = Lens.field @"serverCertificateArn"
{-# INLINEABLE mcveServerCertificateArn #-}
{-# DEPRECATED serverCertificateArn "Use generic-lens or generic-optics with 'serverCertificateArn' instead"  #-}

-- | Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSplitTunnel :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Bool)
mcveSplitTunnel = Lens.field @"splitTunnel"
{-# INLINEABLE mcveSplitTunnel #-}
{-# DEPRECATED splitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead"  #-}

-- | The ID of the VPC to associate with the Client VPN endpoint.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveVpcId :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Types.VpcId)
mcveVpcId = Lens.field @"vpcId"
{-# INLINEABLE mcveVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@ 
-- Default Value: @443@ 
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveVpnPort :: Lens.Lens' ModifyClientVpnEndpoint (Core.Maybe Core.Int)
mcveVpnPort = Lens.field @"vpnPort"
{-# INLINEABLE mcveVpnPort #-}
{-# DEPRECATED vpnPort "Use generic-lens or generic-optics with 'vpnPort' instead"  #-}

instance Core.ToQuery ModifyClientVpnEndpoint where
        toQuery ModifyClientVpnEndpoint{..}
          = Core.toQueryPair "Action"
              ("ModifyClientVpnEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientConnectOptions")
                clientConnectOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConnectionLogOptions")
                connectionLogOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DnsServers") dnsServers
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                securityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SelfServicePortal")
                selfServicePortal
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServerCertificateArn")
                serverCertificateArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SplitTunnel") splitTunnel
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpnPort") vpnPort

instance Core.ToHeaders ModifyClientVpnEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClientVpnEndpoint where
        type Rs ModifyClientVpnEndpoint = ModifyClientVpnEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyClientVpnEndpointResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClientVpnEndpointResponse' smart constructor.
data ModifyClientVpnEndpointResponse = ModifyClientVpnEndpointResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClientVpnEndpointResponse' value with any optional fields omitted.
mkModifyClientVpnEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClientVpnEndpointResponse
mkModifyClientVpnEndpointResponse responseStatus
  = ModifyClientVpnEndpointResponse'{return = Core.Nothing,
                                     responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcverrsReturn :: Lens.Lens' ModifyClientVpnEndpointResponse (Core.Maybe Core.Bool)
mcverrsReturn = Lens.field @"return"
{-# INLINEABLE mcverrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcverrsResponseStatus :: Lens.Lens' ModifyClientVpnEndpointResponse Core.Int
mcverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
