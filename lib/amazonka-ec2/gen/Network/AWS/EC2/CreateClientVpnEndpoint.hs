{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateClientVpnEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Client VPN endpoint. A Client VPN endpoint is the resource you create and configure to enable and manage client VPN sessions. It is the destination endpoint at which all client VPN sessions are terminated.
module Network.AWS.EC2.CreateClientVpnEndpoint
    (
    -- * Creating a request
      CreateClientVpnEndpoint (..)
    , mkCreateClientVpnEndpoint
    -- ** Request lenses
    , ccveClientCidrBlock
    , ccveServerCertificateArn
    , ccveAuthenticationOptions
    , ccveConnectionLogOptions
    , ccveClientConnectOptions
    , ccveClientToken
    , ccveDescription
    , ccveDnsServers
    , ccveDryRun
    , ccveSecurityGroupIds
    , ccveSelfServicePortal
    , ccveSplitTunnel
    , ccveTagSpecifications
    , ccveTransportProtocol
    , ccveVpcId
    , ccveVpnPort

    -- * Destructuring the response
    , CreateClientVpnEndpointResponse (..)
    , mkCreateClientVpnEndpointResponse
    -- ** Response lenses
    , ccverrsClientVpnEndpointId
    , ccverrsDnsName
    , ccverrsStatus
    , ccverrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateClientVpnEndpoint' smart constructor.
data CreateClientVpnEndpoint = CreateClientVpnEndpoint'
  { clientCidrBlock :: Core.Text
    -- ^ The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
  , serverCertificateArn :: Core.Text
    -- ^ The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
  , authenticationOptions :: [Types.ClientVpnAuthenticationRequest]
    -- ^ Information about the authentication method to be used to authenticate clients.
  , connectionLogOptions :: Types.ConnectionLogOptions
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
  , clientConnectOptions :: Core.Maybe Types.ClientConnectOptions
    -- ^ The options for managing connection authorization for new client connections.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the Client VPN endpoint.
  , dnsServers :: Core.Maybe [Core.Text]
    -- ^ Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
  , selfServicePortal :: Core.Maybe Types.SelfServicePortal
    -- ^ Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- Default Value: @enabled@ 
  , splitTunnel :: Core.Maybe Core.Bool
    -- ^ Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the Client VPN endpoint during creation.
  , transportProtocol :: Core.Maybe Types.TransportProtocol
    -- ^ The transport protocol to be used by the VPN session.
--
-- Default value: @udp@ 
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
  , vpnPort :: Core.Maybe Core.Int
    -- ^ The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@ 
-- Default Value: @443@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClientVpnEndpoint' value with any optional fields omitted.
mkCreateClientVpnEndpoint
    :: Core.Text -- ^ 'clientCidrBlock'
    -> Core.Text -- ^ 'serverCertificateArn'
    -> Types.ConnectionLogOptions -- ^ 'connectionLogOptions'
    -> CreateClientVpnEndpoint
mkCreateClientVpnEndpoint clientCidrBlock serverCertificateArn
  connectionLogOptions
  = CreateClientVpnEndpoint'{clientCidrBlock, serverCertificateArn,
                             authenticationOptions = Core.mempty, connectionLogOptions,
                             clientConnectOptions = Core.Nothing, clientToken = Core.Nothing,
                             description = Core.Nothing, dnsServers = Core.Nothing,
                             dryRun = Core.Nothing, securityGroupIds = Core.Nothing,
                             selfServicePortal = Core.Nothing, splitTunnel = Core.Nothing,
                             tagSpecifications = Core.Nothing, transportProtocol = Core.Nothing,
                             vpcId = Core.Nothing, vpnPort = Core.Nothing}

-- | The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
--
-- /Note:/ Consider using 'clientCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientCidrBlock :: Lens.Lens' CreateClientVpnEndpoint Core.Text
ccveClientCidrBlock = Lens.field @"clientCidrBlock"
{-# INLINEABLE ccveClientCidrBlock #-}
{-# DEPRECATED clientCidrBlock "Use generic-lens or generic-optics with 'clientCidrBlock' instead"  #-}

-- | The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
--
-- /Note:/ Consider using 'serverCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveServerCertificateArn :: Lens.Lens' CreateClientVpnEndpoint Core.Text
ccveServerCertificateArn = Lens.field @"serverCertificateArn"
{-# INLINEABLE ccveServerCertificateArn #-}
{-# DEPRECATED serverCertificateArn "Use generic-lens or generic-optics with 'serverCertificateArn' instead"  #-}

-- | Information about the authentication method to be used to authenticate clients.
--
-- /Note:/ Consider using 'authenticationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveAuthenticationOptions :: Lens.Lens' CreateClientVpnEndpoint [Types.ClientVpnAuthenticationRequest]
ccveAuthenticationOptions = Lens.field @"authenticationOptions"
{-# INLINEABLE ccveAuthenticationOptions #-}
{-# DEPRECATED authenticationOptions "Use generic-lens or generic-optics with 'authenticationOptions' instead"  #-}

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
ccveConnectionLogOptions :: Lens.Lens' CreateClientVpnEndpoint Types.ConnectionLogOptions
ccveConnectionLogOptions = Lens.field @"connectionLogOptions"
{-# INLINEABLE ccveConnectionLogOptions #-}
{-# DEPRECATED connectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead"  #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientConnectOptions :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Types.ClientConnectOptions)
ccveClientConnectOptions = Lens.field @"clientConnectOptions"
{-# INLINEABLE ccveClientConnectOptions #-}
{-# DEPRECATED clientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientToken :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Text)
ccveClientToken = Lens.field @"clientToken"
{-# INLINEABLE ccveClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A brief description of the Client VPN endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDescription :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Text)
ccveDescription = Lens.field @"description"
{-# INLINEABLE ccveDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDnsServers :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [Core.Text])
ccveDnsServers = Lens.field @"dnsServers"
{-# INLINEABLE ccveDnsServers #-}
{-# DEPRECATED dnsServers "Use generic-lens or generic-optics with 'dnsServers' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDryRun :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Bool)
ccveDryRun = Lens.field @"dryRun"
{-# INLINEABLE ccveDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSecurityGroupIds :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [Types.SecurityGroupId])
ccveSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE ccveSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- Default Value: @enabled@ 
--
-- /Note:/ Consider using 'selfServicePortal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSelfServicePortal :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Types.SelfServicePortal)
ccveSelfServicePortal = Lens.field @"selfServicePortal"
{-# INLINEABLE ccveSelfServicePortal #-}
{-# DEPRECATED selfServicePortal "Use generic-lens or generic-optics with 'selfServicePortal' instead"  #-}

-- | Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSplitTunnel :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Bool)
ccveSplitTunnel = Lens.field @"splitTunnel"
{-# INLINEABLE ccveSplitTunnel #-}
{-# DEPRECATED splitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead"  #-}

-- | The tags to apply to the Client VPN endpoint during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveTagSpecifications :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe [Types.TagSpecification])
ccveTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ccveTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The transport protocol to be used by the VPN session.
--
-- Default value: @udp@ 
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveTransportProtocol :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Types.TransportProtocol)
ccveTransportProtocol = Lens.field @"transportProtocol"
{-# INLINEABLE ccveTransportProtocol #-}
{-# DEPRECATED transportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead"  #-}

-- | The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveVpcId :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Types.VpcId)
ccveVpcId = Lens.field @"vpcId"
{-# INLINEABLE ccveVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@ 
-- Default Value: @443@ 
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveVpnPort :: Lens.Lens' CreateClientVpnEndpoint (Core.Maybe Core.Int)
ccveVpnPort = Lens.field @"vpnPort"
{-# INLINEABLE ccveVpnPort #-}
{-# DEPRECATED vpnPort "Use generic-lens or generic-optics with 'vpnPort' instead"  #-}

instance Core.ToQuery CreateClientVpnEndpoint where
        toQuery CreateClientVpnEndpoint{..}
          = Core.toQueryPair "Action"
              ("CreateClientVpnEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientCidrBlock" clientCidrBlock
              Core.<>
              Core.toQueryPair "ServerCertificateArn" serverCertificateArn
              Core.<> Core.toQueryList "Authentication" authenticationOptions
              Core.<>
              Core.toQueryPair "ConnectionLogOptions" connectionLogOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientConnectOptions")
                clientConnectOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "DnsServers") dnsServers
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                securityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SelfServicePortal")
                selfServicePortal
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SplitTunnel") splitTunnel
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TransportProtocol")
                transportProtocol
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpnPort") vpnPort

instance Core.ToHeaders CreateClientVpnEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateClientVpnEndpoint where
        type Rs CreateClientVpnEndpoint = CreateClientVpnEndpointResponse
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
                 CreateClientVpnEndpointResponse' Core.<$>
                   (x Core..@? "clientVpnEndpointId") Core.<*> x Core..@? "dnsName"
                     Core.<*> x Core..@? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClientVpnEndpointResponse' smart constructor.
data CreateClientVpnEndpointResponse = CreateClientVpnEndpointResponse'
  { clientVpnEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the Client VPN endpoint.
  , dnsName :: Core.Maybe Core.Text
    -- ^ The DNS name to be used by clients when establishing their VPN session.
  , status :: Core.Maybe Types.ClientVpnEndpointStatus
    -- ^ The current state of the Client VPN endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClientVpnEndpointResponse' value with any optional fields omitted.
mkCreateClientVpnEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClientVpnEndpointResponse
mkCreateClientVpnEndpointResponse responseStatus
  = CreateClientVpnEndpointResponse'{clientVpnEndpointId =
                                       Core.Nothing,
                                     dnsName = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccverrsClientVpnEndpointId :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe Core.Text)
ccverrsClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE ccverrsClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The DNS name to be used by clients when establishing their VPN session.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccverrsDnsName :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe Core.Text)
ccverrsDnsName = Lens.field @"dnsName"
{-# INLINEABLE ccverrsDnsName #-}
{-# DEPRECATED dnsName "Use generic-lens or generic-optics with 'dnsName' instead"  #-}

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccverrsStatus :: Lens.Lens' CreateClientVpnEndpointResponse (Core.Maybe Types.ClientVpnEndpointStatus)
ccverrsStatus = Lens.field @"status"
{-# INLINEABLE ccverrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccverrsResponseStatus :: Lens.Lens' CreateClientVpnEndpointResponse Core.Int
ccverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
