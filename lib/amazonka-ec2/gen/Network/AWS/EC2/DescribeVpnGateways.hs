{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpnGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your virtual private gateways.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeVpnGateways
    (
    -- * Creating a request
      DescribeVpnGateways (..)
    , mkDescribeVpnGateways
    -- ** Request lenses
    , dvgsDryRun
    , dvgsFilters
    , dvgsVpnGatewayIds

    -- * Destructuring the response
    , DescribeVpnGatewaysResponse (..)
    , mkDescribeVpnGatewaysResponse
    -- ** Response lenses
    , dvgrrsVpnGateways
    , dvgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpnGateways.
--
-- /See:/ 'mkDescribeVpnGateways' smart constructor.
data DescribeVpnGateways = DescribeVpnGateways'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @amazon-side-asn@ - The Autonomous System Number (ASN) for the Amazon side of the gateway.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@attaching@ | @attached@ | @detaching@ | @detached@ ).
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @availability-zone@ - The Availability Zone for the virtual private gateway (if applicable).
--
--
--     * @state@ - The state of the virtual private gateway (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of virtual private gateway. Currently the only supported type is @ipsec.1@ .
--
--
--     * @vpn-gateway-id@ - The ID of the virtual private gateway.
--
--
  , vpnGatewayIds :: Core.Maybe [Types.VpnGatewayId]
    -- ^ One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpnGateways' value with any optional fields omitted.
mkDescribeVpnGateways
    :: DescribeVpnGateways
mkDescribeVpnGateways
  = DescribeVpnGateways'{dryRun = Core.Nothing,
                         filters = Core.Nothing, vpnGatewayIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgsDryRun :: Lens.Lens' DescribeVpnGateways (Core.Maybe Core.Bool)
dvgsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvgsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @amazon-side-asn@ - The Autonomous System Number (ASN) for the Amazon side of the gateway.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@attaching@ | @attached@ | @detaching@ | @detached@ ).
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @availability-zone@ - The Availability Zone for the virtual private gateway (if applicable).
--
--
--     * @state@ - The state of the virtual private gateway (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of virtual private gateway. Currently the only supported type is @ipsec.1@ .
--
--
--     * @vpn-gateway-id@ - The ID of the virtual private gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgsFilters :: Lens.Lens' DescribeVpnGateways (Core.Maybe [Types.Filter])
dvgsFilters = Lens.field @"filters"
{-# INLINEABLE dvgsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
--
-- /Note:/ Consider using 'vpnGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgsVpnGatewayIds :: Lens.Lens' DescribeVpnGateways (Core.Maybe [Types.VpnGatewayId])
dvgsVpnGatewayIds = Lens.field @"vpnGatewayIds"
{-# INLINEABLE dvgsVpnGatewayIds #-}
{-# DEPRECATED vpnGatewayIds "Use generic-lens or generic-optics with 'vpnGatewayIds' instead"  #-}

instance Core.ToQuery DescribeVpnGateways where
        toQuery DescribeVpnGateways{..}
          = Core.toQueryPair "Action" ("DescribeVpnGateways" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "VpnGatewayId")
                vpnGatewayIds

instance Core.ToHeaders DescribeVpnGateways where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpnGateways where
        type Rs DescribeVpnGateways = DescribeVpnGatewaysResponse
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
                 DescribeVpnGatewaysResponse' Core.<$>
                   (x Core..@? "vpnGatewaySet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DescribeVpnGateways.
--
-- /See:/ 'mkDescribeVpnGatewaysResponse' smart constructor.
data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse'
  { vpnGateways :: Core.Maybe [Types.VpnGateway]
    -- ^ Information about one or more virtual private gateways.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpnGatewaysResponse' value with any optional fields omitted.
mkDescribeVpnGatewaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpnGatewaysResponse
mkDescribeVpnGatewaysResponse responseStatus
  = DescribeVpnGatewaysResponse'{vpnGateways = Core.Nothing,
                                 responseStatus}

-- | Information about one or more virtual private gateways.
--
-- /Note:/ Consider using 'vpnGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrrsVpnGateways :: Lens.Lens' DescribeVpnGatewaysResponse (Core.Maybe [Types.VpnGateway])
dvgrrsVpnGateways = Lens.field @"vpnGateways"
{-# INLINEABLE dvgrrsVpnGateways #-}
{-# DEPRECATED vpnGateways "Use generic-lens or generic-optics with 'vpnGateways' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrrsResponseStatus :: Lens.Lens' DescribeVpnGatewaysResponse Core.Int
dvgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
