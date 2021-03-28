{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN customer gateways.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeCustomerGateways
    (
    -- * Creating a request
      DescribeCustomerGateways (..)
    , mkDescribeCustomerGateways
    -- ** Request lenses
    , dcgCustomerGatewayIds
    , dcgDryRun
    , dcgFilters

    -- * Destructuring the response
    , DescribeCustomerGatewaysResponse (..)
    , mkDescribeCustomerGatewaysResponse
    -- ** Response lenses
    , dcgrrsCustomerGateways
    , dcgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
  { customerGatewayIds :: Core.Maybe [Types.CustomerGatewayId]
    -- ^ One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @bgp-asn@ - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
--
--     * @customer-gateway-id@ - The ID of the customer gateway.
--
--
--     * @ip-address@ - The IP address of the customer gateway's Internet-routable external interface.
--
--
--     * @state@ - The state of the customer gateway (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @type@ - The type of customer gateway. Currently, the only supported type is @ipsec.1@ .
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomerGateways' value with any optional fields omitted.
mkDescribeCustomerGateways
    :: DescribeCustomerGateways
mkDescribeCustomerGateways
  = DescribeCustomerGateways'{customerGatewayIds = Core.Nothing,
                              dryRun = Core.Nothing, filters = Core.Nothing}

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
--
-- /Note:/ Consider using 'customerGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgCustomerGatewayIds :: Lens.Lens' DescribeCustomerGateways (Core.Maybe [Types.CustomerGatewayId])
dcgCustomerGatewayIds = Lens.field @"customerGatewayIds"
{-# INLINEABLE dcgCustomerGatewayIds #-}
{-# DEPRECATED customerGatewayIds "Use generic-lens or generic-optics with 'customerGatewayIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDryRun :: Lens.Lens' DescribeCustomerGateways (Core.Maybe Core.Bool)
dcgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @bgp-asn@ - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
--
--     * @customer-gateway-id@ - The ID of the customer gateway.
--
--
--     * @ip-address@ - The IP address of the customer gateway's Internet-routable external interface.
--
--
--     * @state@ - The state of the customer gateway (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @type@ - The type of customer gateway. Currently, the only supported type is @ipsec.1@ .
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgFilters :: Lens.Lens' DescribeCustomerGateways (Core.Maybe [Types.Filter])
dcgFilters = Lens.field @"filters"
{-# INLINEABLE dcgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

instance Core.ToQuery DescribeCustomerGateways where
        toQuery DescribeCustomerGateways{..}
          = Core.toQueryPair "Action"
              ("DescribeCustomerGateways" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "CustomerGatewayId")
                customerGatewayIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters

instance Core.ToHeaders DescribeCustomerGateways where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCustomerGateways where
        type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse
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
                 DescribeCustomerGatewaysResponse' Core.<$>
                   (x Core..@? "customerGatewaySet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
  { customerGateways :: Core.Maybe [Types.CustomerGateway]
    -- ^ Information about one or more customer gateways.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomerGatewaysResponse' value with any optional fields omitted.
mkDescribeCustomerGatewaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCustomerGatewaysResponse
mkDescribeCustomerGatewaysResponse responseStatus
  = DescribeCustomerGatewaysResponse'{customerGateways =
                                        Core.Nothing,
                                      responseStatus}

-- | Information about one or more customer gateways.
--
-- /Note:/ Consider using 'customerGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrrsCustomerGateways :: Lens.Lens' DescribeCustomerGatewaysResponse (Core.Maybe [Types.CustomerGateway])
dcgrrsCustomerGateways = Lens.field @"customerGateways"
{-# INLINEABLE dcgrrsCustomerGateways #-}
{-# DEPRECATED customerGateways "Use generic-lens or generic-optics with 'customerGateways' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrrsResponseStatus :: Lens.Lens' DescribeCustomerGatewaysResponse Core.Int
dcgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
