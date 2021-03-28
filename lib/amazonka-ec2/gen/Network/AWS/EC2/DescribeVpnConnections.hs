{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpnConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN connections.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeVpnConnections
    (
    -- * Creating a request
      DescribeVpnConnections (..)
    , mkDescribeVpnConnections
    -- ** Request lenses
    , dvcsDryRun
    , dvcsFilters
    , dvcsVpnConnectionIds

    -- * Destructuring the response
    , DescribeVpnConnectionsResponse (..)
    , mkDescribeVpnConnectionsResponse
    -- ** Response lenses
    , dvcrrsVpnConnections
    , dvcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpnConnections.
--
-- /See:/ 'mkDescribeVpnConnections' smart constructor.
data DescribeVpnConnections = DescribeVpnConnections'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @customer-gateway-configuration@ - The configuration information for the customer gateway.
--
--
--     * @customer-gateway-id@ - The ID of a customer gateway associated with the VPN connection.
--
--
--     * @state@ - The state of the VPN connection (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @option.static-routes-only@ - Indicates whether the connection has static routes only. Used for devices that do not support Border Gateway Protocol (BGP).
--
--
--     * @route.destination-cidr-block@ - The destination CIDR block. This corresponds to the subnet used in a customer data center.
--
--
--     * @bgp-asn@ - The BGP Autonomous System Number (ASN) associated with a BGP device.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of VPN connection. Currently the only supported type is @ipsec.1@ .
--
--
--     * @vpn-connection-id@ - The ID of the VPN connection.
--
--
--     * @vpn-gateway-id@ - The ID of a virtual private gateway associated with the VPN connection.
--
--
--     * @transit-gateway-id@ - The ID of a transit gateway associated with the VPN connection.
--
--
  , vpnConnectionIds :: Core.Maybe [Types.VpnConnectionId]
    -- ^ One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpnConnections' value with any optional fields omitted.
mkDescribeVpnConnections
    :: DescribeVpnConnections
mkDescribeVpnConnections
  = DescribeVpnConnections'{dryRun = Core.Nothing,
                            filters = Core.Nothing, vpnConnectionIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcsDryRun :: Lens.Lens' DescribeVpnConnections (Core.Maybe Core.Bool)
dvcsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvcsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @customer-gateway-configuration@ - The configuration information for the customer gateway.
--
--
--     * @customer-gateway-id@ - The ID of a customer gateway associated with the VPN connection.
--
--
--     * @state@ - The state of the VPN connection (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @option.static-routes-only@ - Indicates whether the connection has static routes only. Used for devices that do not support Border Gateway Protocol (BGP).
--
--
--     * @route.destination-cidr-block@ - The destination CIDR block. This corresponds to the subnet used in a customer data center.
--
--
--     * @bgp-asn@ - The BGP Autonomous System Number (ASN) associated with a BGP device.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of VPN connection. Currently the only supported type is @ipsec.1@ .
--
--
--     * @vpn-connection-id@ - The ID of the VPN connection.
--
--
--     * @vpn-gateway-id@ - The ID of a virtual private gateway associated with the VPN connection.
--
--
--     * @transit-gateway-id@ - The ID of a transit gateway associated with the VPN connection.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcsFilters :: Lens.Lens' DescribeVpnConnections (Core.Maybe [Types.Filter])
dvcsFilters = Lens.field @"filters"
{-# INLINEABLE dvcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
--
-- /Note:/ Consider using 'vpnConnectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcsVpnConnectionIds :: Lens.Lens' DescribeVpnConnections (Core.Maybe [Types.VpnConnectionId])
dvcsVpnConnectionIds = Lens.field @"vpnConnectionIds"
{-# INLINEABLE dvcsVpnConnectionIds #-}
{-# DEPRECATED vpnConnectionIds "Use generic-lens or generic-optics with 'vpnConnectionIds' instead"  #-}

instance Core.ToQuery DescribeVpnConnections where
        toQuery DescribeVpnConnections{..}
          = Core.toQueryPair "Action" ("DescribeVpnConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "VpnConnectionId")
                vpnConnectionIds

instance Core.ToHeaders DescribeVpnConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpnConnections where
        type Rs DescribeVpnConnections = DescribeVpnConnectionsResponse
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
                 DescribeVpnConnectionsResponse' Core.<$>
                   (x Core..@? "vpnConnectionSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DescribeVpnConnections.
--
-- /See:/ 'mkDescribeVpnConnectionsResponse' smart constructor.
data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse'
  { vpnConnections :: Core.Maybe [Types.VpnConnection]
    -- ^ Information about one or more VPN connections.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVpnConnectionsResponse' value with any optional fields omitted.
mkDescribeVpnConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpnConnectionsResponse
mkDescribeVpnConnectionsResponse responseStatus
  = DescribeVpnConnectionsResponse'{vpnConnections = Core.Nothing,
                                    responseStatus}

-- | Information about one or more VPN connections.
--
-- /Note:/ Consider using 'vpnConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrrsVpnConnections :: Lens.Lens' DescribeVpnConnectionsResponse (Core.Maybe [Types.VpnConnection])
dvcrrsVpnConnections = Lens.field @"vpnConnections"
{-# INLINEABLE dvcrrsVpnConnections #-}
{-# DEPRECATED vpnConnections "Use generic-lens or generic-optics with 'vpnConnections' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrrsResponseStatus :: Lens.Lens' DescribeVpnConnectionsResponse Core.Int
dvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
