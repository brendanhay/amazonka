{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPNConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN connections.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeVPNConnections
  ( -- * Creating a request
    DescribeVPNConnections (..),
    mkDescribeVPNConnections,

    -- ** Request lenses
    dvpncFilters,
    dvpncVPNConnectionIds,
    dvpncDryRun,

    -- * Destructuring the response
    DescribeVPNConnectionsResponse (..),
    mkDescribeVPNConnectionsResponse,

    -- ** Response lenses
    dvcrsVPNConnections,
    dvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeVpnConnections.
--
-- /See:/ 'mkDescribeVPNConnections' smart constructor.
data DescribeVPNConnections = DescribeVPNConnections'
  { filters ::
      Lude.Maybe [Filter],
    vpnConnectionIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPNConnections' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
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
-- * 'vpnConnectionIds' - One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
mkDescribeVPNConnections ::
  DescribeVPNConnections
mkDescribeVPNConnections =
  DescribeVPNConnections'
    { filters = Lude.Nothing,
      vpnConnectionIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

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
dvpncFilters :: Lens.Lens' DescribeVPNConnections (Lude.Maybe [Filter])
dvpncFilters = Lens.lens (filters :: DescribeVPNConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPNConnections)
{-# DEPRECATED dvpncFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
--
-- /Note:/ Consider using 'vpnConnectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpncVPNConnectionIds :: Lens.Lens' DescribeVPNConnections (Lude.Maybe [Lude.Text])
dvpncVPNConnectionIds = Lens.lens (vpnConnectionIds :: DescribeVPNConnections -> Lude.Maybe [Lude.Text]) (\s a -> s {vpnConnectionIds = a} :: DescribeVPNConnections)
{-# DEPRECATED dvpncVPNConnectionIds "Use generic-lens or generic-optics with 'vpnConnectionIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpncDryRun :: Lens.Lens' DescribeVPNConnections (Lude.Maybe Lude.Bool)
dvpncDryRun = Lens.lens (dryRun :: DescribeVPNConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPNConnections)
{-# DEPRECATED dvpncDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeVPNConnections where
  type Rs DescribeVPNConnections = DescribeVPNConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPNConnectionsResponse'
            Lude.<$> ( x Lude..@? "vpnConnectionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPNConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPNConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPNConnections where
  toQuery DescribeVPNConnections' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpnConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "VpnConnectionId" Lude.<$> vpnConnectionIds),
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of DescribeVpnConnections.
--
-- /See:/ 'mkDescribeVPNConnectionsResponse' smart constructor.
data DescribeVPNConnectionsResponse = DescribeVPNConnectionsResponse'
  { vpnConnections ::
      Lude.Maybe [VPNConnection],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPNConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpnConnections' - Information about one or more VPN connections.
mkDescribeVPNConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPNConnectionsResponse
mkDescribeVPNConnectionsResponse pResponseStatus_ =
  DescribeVPNConnectionsResponse'
    { vpnConnections = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more VPN connections.
--
-- /Note:/ Consider using 'vpnConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrsVPNConnections :: Lens.Lens' DescribeVPNConnectionsResponse (Lude.Maybe [VPNConnection])
dvcrsVPNConnections = Lens.lens (vpnConnections :: DescribeVPNConnectionsResponse -> Lude.Maybe [VPNConnection]) (\s a -> s {vpnConnections = a} :: DescribeVPNConnectionsResponse)
{-# DEPRECATED dvcrsVPNConnections "Use generic-lens or generic-optics with 'vpnConnections' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrsResponseStatus :: Lens.Lens' DescribeVPNConnectionsResponse Lude.Int
dvcrsResponseStatus = Lens.lens (responseStatus :: DescribeVPNConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPNConnectionsResponse)
{-# DEPRECATED dvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
