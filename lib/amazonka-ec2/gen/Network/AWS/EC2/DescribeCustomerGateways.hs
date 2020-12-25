{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeCustomerGateways (..),
    mkDescribeCustomerGateways,

    -- ** Request lenses
    dcgCustomerGatewayIds,
    dcgDryRun,
    dcgFilters,

    -- * Destructuring the response
    DescribeCustomerGatewaysResponse (..),
    mkDescribeCustomerGatewaysResponse,

    -- ** Response lenses
    dcgrrsCustomerGateways,
    dcgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
  { -- | One or more customer gateway IDs.
    --
    -- Default: Describes all your customer gateways.
    customerGatewayIds :: Core.Maybe [Types.CustomerGatewayId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomerGateways' value with any optional fields omitted.
mkDescribeCustomerGateways ::
  DescribeCustomerGateways
mkDescribeCustomerGateways =
  DescribeCustomerGateways'
    { customerGatewayIds = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing
    }

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
--
-- /Note:/ Consider using 'customerGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgCustomerGatewayIds :: Lens.Lens' DescribeCustomerGateways (Core.Maybe [Types.CustomerGatewayId])
dcgCustomerGatewayIds = Lens.field @"customerGatewayIds"
{-# DEPRECATED dcgCustomerGatewayIds "Use generic-lens or generic-optics with 'customerGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDryRun :: Lens.Lens' DescribeCustomerGateways (Core.Maybe Core.Bool)
dcgDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED dcgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.AWSRequest DescribeCustomerGateways where
  type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeCustomerGateways")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "CustomerGatewayId" Core.<$> customerGatewayIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCustomerGatewaysResponse'
            Core.<$> ( x Core..@? "customerGatewaySet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
  { -- | Information about one or more customer gateways.
    customerGateways :: Core.Maybe [Types.CustomerGateway],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomerGatewaysResponse' value with any optional fields omitted.
mkDescribeCustomerGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCustomerGatewaysResponse
mkDescribeCustomerGatewaysResponse responseStatus =
  DescribeCustomerGatewaysResponse'
    { customerGateways =
        Core.Nothing,
      responseStatus
    }

-- | Information about one or more customer gateways.
--
-- /Note:/ Consider using 'customerGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrrsCustomerGateways :: Lens.Lens' DescribeCustomerGatewaysResponse (Core.Maybe [Types.CustomerGateway])
dcgrrsCustomerGateways = Lens.field @"customerGateways"
{-# DEPRECATED dcgrrsCustomerGateways "Use generic-lens or generic-optics with 'customerGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrrsResponseStatus :: Lens.Lens' DescribeCustomerGatewaysResponse Core.Int
dcgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
