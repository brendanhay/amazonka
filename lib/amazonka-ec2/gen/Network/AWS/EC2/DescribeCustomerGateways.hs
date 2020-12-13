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
    dcgFilters,
    dcgDryRun,

    -- * Destructuring the response
    DescribeCustomerGatewaysResponse (..),
    mkDescribeCustomerGatewaysResponse,

    -- ** Response lenses
    dcgfrsCustomerGateways,
    dcgfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
  { -- | One or more customer gateway IDs.
    --
    -- Default: Describes all your customer gateways.
    customerGatewayIds :: Lude.Maybe [Lude.Text],
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
    filters :: Lude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCustomerGateways' with the minimum fields required to make a request.
--
-- * 'customerGatewayIds' - One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
-- * 'filters' - One or more filters.
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
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeCustomerGateways ::
  DescribeCustomerGateways
mkDescribeCustomerGateways =
  DescribeCustomerGateways'
    { customerGatewayIds = Lude.Nothing,
      filters = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
--
-- /Note:/ Consider using 'customerGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgCustomerGatewayIds :: Lens.Lens' DescribeCustomerGateways (Lude.Maybe [Lude.Text])
dcgCustomerGatewayIds = Lens.lens (customerGatewayIds :: DescribeCustomerGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {customerGatewayIds = a} :: DescribeCustomerGateways)
{-# DEPRECATED dcgCustomerGatewayIds "Use generic-lens or generic-optics with 'customerGatewayIds' instead." #-}

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
dcgFilters :: Lens.Lens' DescribeCustomerGateways (Lude.Maybe [Filter])
dcgFilters = Lens.lens (filters :: DescribeCustomerGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCustomerGateways)
{-# DEPRECATED dcgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDryRun :: Lens.Lens' DescribeCustomerGateways (Lude.Maybe Lude.Bool)
dcgDryRun = Lens.lens (dryRun :: DescribeCustomerGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeCustomerGateways)
{-# DEPRECATED dcgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeCustomerGateways where
  type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeCustomerGatewaysResponse'
            Lude.<$> ( x Lude..@? "customerGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCustomerGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCustomerGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCustomerGateways where
  toQuery DescribeCustomerGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCustomerGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "CustomerGatewayId" Lude.<$> customerGatewayIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of DescribeCustomerGateways.
--
-- /See:/ 'mkDescribeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
  { -- | Information about one or more customer gateways.
    customerGateways :: Lude.Maybe [CustomerGateway],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCustomerGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'customerGateways' - Information about one or more customer gateways.
-- * 'responseStatus' - The response status code.
mkDescribeCustomerGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCustomerGatewaysResponse
mkDescribeCustomerGatewaysResponse pResponseStatus_ =
  DescribeCustomerGatewaysResponse'
    { customerGateways =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more customer gateways.
--
-- /Note:/ Consider using 'customerGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfrsCustomerGateways :: Lens.Lens' DescribeCustomerGatewaysResponse (Lude.Maybe [CustomerGateway])
dcgfrsCustomerGateways = Lens.lens (customerGateways :: DescribeCustomerGatewaysResponse -> Lude.Maybe [CustomerGateway]) (\s a -> s {customerGateways = a} :: DescribeCustomerGatewaysResponse)
{-# DEPRECATED dcgfrsCustomerGateways "Use generic-lens or generic-optics with 'customerGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfrsResponseStatus :: Lens.Lens' DescribeCustomerGatewaysResponse Lude.Int
dcgfrsResponseStatus = Lens.lens (responseStatus :: DescribeCustomerGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCustomerGatewaysResponse)
{-# DEPRECATED dcgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
