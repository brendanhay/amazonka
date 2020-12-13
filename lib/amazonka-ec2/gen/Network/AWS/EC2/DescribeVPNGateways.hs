{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPNGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your virtual private gateways.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeVPNGateways
  ( -- * Creating a request
    DescribeVPNGateways (..),
    mkDescribeVPNGateways,

    -- ** Request lenses
    dvgsFilters,
    dvgsVPNGatewayIds,
    dvgsDryRun,

    -- * Destructuring the response
    DescribeVPNGatewaysResponse (..),
    mkDescribeVPNGatewaysResponse,

    -- ** Response lenses
    dvgrsVPNGateways,
    dvgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeVpnGateways.
--
-- /See:/ 'mkDescribeVPNGateways' smart constructor.
data DescribeVPNGateways = DescribeVPNGateways'
  { -- | One or more filters.
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
    filters :: Lude.Maybe [Filter],
    -- | One or more virtual private gateway IDs.
    --
    -- Default: Describes all your virtual private gateways.
    vpnGatewayIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPNGateways' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
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
-- * 'vpnGatewayIds' - One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeVPNGateways ::
  DescribeVPNGateways
mkDescribeVPNGateways =
  DescribeVPNGateways'
    { filters = Lude.Nothing,
      vpnGatewayIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

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
dvgsFilters :: Lens.Lens' DescribeVPNGateways (Lude.Maybe [Filter])
dvgsFilters = Lens.lens (filters :: DescribeVPNGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPNGateways)
{-# DEPRECATED dvgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
--
-- /Note:/ Consider using 'vpnGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgsVPNGatewayIds :: Lens.Lens' DescribeVPNGateways (Lude.Maybe [Lude.Text])
dvgsVPNGatewayIds = Lens.lens (vpnGatewayIds :: DescribeVPNGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {vpnGatewayIds = a} :: DescribeVPNGateways)
{-# DEPRECATED dvgsVPNGatewayIds "Use generic-lens or generic-optics with 'vpnGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgsDryRun :: Lens.Lens' DescribeVPNGateways (Lude.Maybe Lude.Bool)
dvgsDryRun = Lens.lens (dryRun :: DescribeVPNGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPNGateways)
{-# DEPRECATED dvgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeVPNGateways where
  type Rs DescribeVPNGateways = DescribeVPNGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPNGatewaysResponse'
            Lude.<$> ( x Lude..@? "vpnGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPNGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPNGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPNGateways where
  toQuery DescribeVPNGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpnGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "VpnGatewayId" Lude.<$> vpnGatewayIds),
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of DescribeVpnGateways.
--
-- /See:/ 'mkDescribeVPNGatewaysResponse' smart constructor.
data DescribeVPNGatewaysResponse = DescribeVPNGatewaysResponse'
  { -- | Information about one or more virtual private gateways.
    vpnGateways :: Lude.Maybe [VPNGateway],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPNGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'vpnGateways' - Information about one or more virtual private gateways.
-- * 'responseStatus' - The response status code.
mkDescribeVPNGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPNGatewaysResponse
mkDescribeVPNGatewaysResponse pResponseStatus_ =
  DescribeVPNGatewaysResponse'
    { vpnGateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more virtual private gateways.
--
-- /Note:/ Consider using 'vpnGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrsVPNGateways :: Lens.Lens' DescribeVPNGatewaysResponse (Lude.Maybe [VPNGateway])
dvgrsVPNGateways = Lens.lens (vpnGateways :: DescribeVPNGatewaysResponse -> Lude.Maybe [VPNGateway]) (\s a -> s {vpnGateways = a} :: DescribeVPNGatewaysResponse)
{-# DEPRECATED dvgrsVPNGateways "Use generic-lens or generic-optics with 'vpnGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrsResponseStatus :: Lens.Lens' DescribeVPNGatewaysResponse Lude.Int
dvgrsResponseStatus = Lens.lens (responseStatus :: DescribeVPNGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPNGatewaysResponse)
{-# DEPRECATED dvgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
