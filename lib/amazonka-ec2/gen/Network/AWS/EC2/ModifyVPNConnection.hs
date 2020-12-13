{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the customer gateway or the target gateway of an AWS Site-to-Site VPN connection. To modify the target gateway, the following migration options are available:
--
--
--     * An existing virtual private gateway to a new virtual private gateway
--
--
--     * An existing virtual private gateway to a transit gateway
--
--
--     * An existing transit gateway to a new transit gateway
--
--
--     * An existing transit gateway to a virtual private gateway
--
--
-- Before you perform the migration to the new gateway, you must configure the new gateway. Use 'CreateVpnGateway' to create a virtual private gateway, or 'CreateTransitGateway' to create a transit gateway.
-- This step is required when you migrate from a virtual private gateway with static routes to a transit gateway.
-- You must delete the static routes before you migrate to the new gateway.
-- Keep a copy of the static route before you delete it. You will need to add back these routes to the transit gateway after the VPN connection migration is complete.
-- After you migrate to the new gateway, you might need to modify your VPC route table. Use 'CreateRoute' and 'DeleteRoute' to make the changes described in <https://docs.aws.amazon.com/vpn/latest/s2svpn/modify-vpn-target.html#step-update-routing VPN Gateway Target Modification Required VPC Route Table Updates> in the /AWS Site-to-Site VPN User Guide/ .
-- When the new gateway is a transit gateway, modify the transit gateway route table to allow traffic between the VPC and the AWS Site-to-Site VPN connection. Use 'CreateTransitGatewayRoute' to add the routes.
-- If you deleted VPN static routes, you must add the static routes to the transit gateway route table.
-- After you perform this operation, the AWS VPN endpoint's IP addresses on the AWS side and the tunnel options remain intact. Your AWS Site-to-Site VPN connection will be temporarily unavailable for a brief period while we provision the new endpoints.
module Network.AWS.EC2.ModifyVPNConnection
  ( -- * Creating a request
    ModifyVPNConnection (..),
    mkModifyVPNConnection,

    -- ** Request lenses
    mvcVPNGatewayId,
    mvcCustomerGatewayId,
    mvcTransitGatewayId,
    mvcVPNConnectionId,
    mvcDryRun,

    -- * Destructuring the response
    ModifyVPNConnectionResponse (..),
    mkModifyVPNConnectionResponse,

    -- ** Response lenses
    mvcrsVPNConnection,
    mvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPNConnection' smart constructor.
data ModifyVPNConnection = ModifyVPNConnection'
  { -- | The ID of the virtual private gateway at the AWS side of the VPN connection.
    vpnGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the customer gateway at your end of the VPN connection.
    customerGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNConnection' with the minimum fields required to make a request.
--
-- * 'vpnGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN connection.
-- * 'customerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'vpnConnectionId' - The ID of the VPN connection.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPNConnection ::
  -- | 'vpnConnectionId'
  Lude.Text ->
  ModifyVPNConnection
mkModifyVPNConnection pVPNConnectionId_ =
  ModifyVPNConnection'
    { vpnGatewayId = Lude.Nothing,
      customerGatewayId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      vpnConnectionId = pVPNConnectionId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcVPNGatewayId :: Lens.Lens' ModifyVPNConnection (Lude.Maybe Lude.Text)
mvcVPNGatewayId = Lens.lens (vpnGatewayId :: ModifyVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpnGatewayId = a} :: ModifyVPNConnection)
{-# DEPRECATED mvcVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The ID of the customer gateway at your end of the VPN connection.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcCustomerGatewayId :: Lens.Lens' ModifyVPNConnection (Lude.Maybe Lude.Text)
mvcCustomerGatewayId = Lens.lens (customerGatewayId :: ModifyVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {customerGatewayId = a} :: ModifyVPNConnection)
{-# DEPRECATED mvcCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcTransitGatewayId :: Lens.Lens' ModifyVPNConnection (Lude.Maybe Lude.Text)
mvcTransitGatewayId = Lens.lens (transitGatewayId :: ModifyVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: ModifyVPNConnection)
{-# DEPRECATED mvcTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcVPNConnectionId :: Lens.Lens' ModifyVPNConnection Lude.Text
mvcVPNConnectionId = Lens.lens (vpnConnectionId :: ModifyVPNConnection -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: ModifyVPNConnection)
{-# DEPRECATED mvcVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcDryRun :: Lens.Lens' ModifyVPNConnection (Lude.Maybe Lude.Bool)
mvcDryRun = Lens.lens (dryRun :: ModifyVPNConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPNConnection)
{-# DEPRECATED mvcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPNConnection where
  type Rs ModifyVPNConnection = ModifyVPNConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPNConnectionResponse'
            Lude.<$> (x Lude..@? "vpnConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPNConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPNConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPNConnection where
  toQuery ModifyVPNConnection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVpnConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnGatewayId" Lude.=: vpnGatewayId,
        "CustomerGatewayId" Lude.=: customerGatewayId,
        "TransitGatewayId" Lude.=: transitGatewayId,
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPNConnectionResponse' smart constructor.
data ModifyVPNConnectionResponse = ModifyVPNConnectionResponse'
  { vpnConnection :: Lude.Maybe VPNConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNConnectionResponse' with the minimum fields required to make a request.
--
-- * 'vpnConnection' -
-- * 'responseStatus' - The response status code.
mkModifyVPNConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPNConnectionResponse
mkModifyVPNConnectionResponse pResponseStatus_ =
  ModifyVPNConnectionResponse'
    { vpnConnection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcrsVPNConnection :: Lens.Lens' ModifyVPNConnectionResponse (Lude.Maybe VPNConnection)
mvcrsVPNConnection = Lens.lens (vpnConnection :: ModifyVPNConnectionResponse -> Lude.Maybe VPNConnection) (\s a -> s {vpnConnection = a} :: ModifyVPNConnectionResponse)
{-# DEPRECATED mvcrsVPNConnection "Use generic-lens or generic-optics with 'vpnConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcrsResponseStatus :: Lens.Lens' ModifyVPNConnectionResponse Lude.Int
mvcrsResponseStatus = Lens.lens (responseStatus :: ModifyVPNConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPNConnectionResponse)
{-# DEPRECATED mvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
