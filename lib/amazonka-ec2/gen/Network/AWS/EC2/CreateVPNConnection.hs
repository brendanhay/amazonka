{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPN connection between an existing virtual private gateway or transit gateway and a customer gateway. The supported connection type is @ipsec.1@ .
--
-- The response includes information that you need to give to your network administrator to configure your customer gateway.
-- /Important:/ We strongly recommend that you use HTTPS when calling this operation because the response contains sensitive cryptographic information for configuring your customer gateway device.
-- If you decide to shut down your VPN connection for any reason and later create a new VPN connection, you must reconfigure your customer gateway with the new information returned from this call.
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVPNConnection
  ( -- * Creating a request
    CreateVPNConnection (..),
    mkCreateVPNConnection,

    -- ** Request lenses
    cvcVPNGatewayId,
    cvcTagSpecifications,
    cvcTransitGatewayId,
    cvcOptions,
    cvcDryRun,
    cvcCustomerGatewayId,
    cvcType,

    -- * Destructuring the response
    CreateVPNConnectionResponse (..),
    mkCreateVPNConnectionResponse,

    -- ** Response lenses
    cvcrsVPNConnection,
    cvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateVpnConnection.
--
-- /See:/ 'mkCreateVPNConnection' smart constructor.
data CreateVPNConnection = CreateVPNConnection'
  { vpnGatewayId ::
      Lude.Maybe Lude.Text,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    transitGatewayId :: Lude.Maybe Lude.Text,
    options ::
      Lude.Maybe VPNConnectionOptionsSpecification,
    dryRun :: Lude.Maybe Lude.Bool,
    customerGatewayId :: Lude.Text,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPNConnection' with the minimum fields required to make a request.
--
-- * 'customerGatewayId' - The ID of the customer gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'options' - The options for the VPN connection.
-- * 'tagSpecifications' - The tags to apply to the VPN connection.
-- * 'transitGatewayId' - The ID of the transit gateway. If you specify a transit gateway, you cannot specify a virtual private gateway.
-- * 'type'' - The type of VPN connection (@ipsec.1@ ).
-- * 'vpnGatewayId' - The ID of the virtual private gateway. If you specify a virtual private gateway, you cannot specify a transit gateway.
mkCreateVPNConnection ::
  -- | 'customerGatewayId'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  CreateVPNConnection
mkCreateVPNConnection pCustomerGatewayId_ pType_ =
  CreateVPNConnection'
    { vpnGatewayId = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      options = Lude.Nothing,
      dryRun = Lude.Nothing,
      customerGatewayId = pCustomerGatewayId_,
      type' = pType_
    }

-- | The ID of the virtual private gateway. If you specify a virtual private gateway, you cannot specify a transit gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcVPNGatewayId :: Lens.Lens' CreateVPNConnection (Lude.Maybe Lude.Text)
cvcVPNGatewayId = Lens.lens (vpnGatewayId :: CreateVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpnGatewayId = a} :: CreateVPNConnection)
{-# DEPRECATED cvcVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The tags to apply to the VPN connection.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTagSpecifications :: Lens.Lens' CreateVPNConnection (Lude.Maybe [TagSpecification])
cvcTagSpecifications = Lens.lens (tagSpecifications :: CreateVPNConnection -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVPNConnection)
{-# DEPRECATED cvcTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The ID of the transit gateway. If you specify a transit gateway, you cannot specify a virtual private gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTransitGatewayId :: Lens.Lens' CreateVPNConnection (Lude.Maybe Lude.Text)
cvcTransitGatewayId = Lens.lens (transitGatewayId :: CreateVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateVPNConnection)
{-# DEPRECATED cvcTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The options for the VPN connection.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcOptions :: Lens.Lens' CreateVPNConnection (Lude.Maybe VPNConnectionOptionsSpecification)
cvcOptions = Lens.lens (options :: CreateVPNConnection -> Lude.Maybe VPNConnectionOptionsSpecification) (\s a -> s {options = a} :: CreateVPNConnection)
{-# DEPRECATED cvcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcDryRun :: Lens.Lens' CreateVPNConnection (Lude.Maybe Lude.Bool)
cvcDryRun = Lens.lens (dryRun :: CreateVPNConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPNConnection)
{-# DEPRECATED cvcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcCustomerGatewayId :: Lens.Lens' CreateVPNConnection Lude.Text
cvcCustomerGatewayId = Lens.lens (customerGatewayId :: CreateVPNConnection -> Lude.Text) (\s a -> s {customerGatewayId = a} :: CreateVPNConnection)
{-# DEPRECATED cvcCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The type of VPN connection (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcType :: Lens.Lens' CreateVPNConnection Lude.Text
cvcType = Lens.lens (type' :: CreateVPNConnection -> Lude.Text) (\s a -> s {type' = a} :: CreateVPNConnection)
{-# DEPRECATED cvcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest CreateVPNConnection where
  type Rs CreateVPNConnection = CreateVPNConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPNConnectionResponse'
            Lude.<$> (x Lude..@? "vpnConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPNConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPNConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPNConnection where
  toQuery CreateVPNConnection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVpnConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnGatewayId" Lude.=: vpnGatewayId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "TransitGatewayId" Lude.=: transitGatewayId,
        "Options" Lude.=: options,
        "DryRun" Lude.=: dryRun,
        "CustomerGatewayId" Lude.=: customerGatewayId,
        "Type" Lude.=: type'
      ]

-- | Contains the output of CreateVpnConnection.
--
-- /See:/ 'mkCreateVPNConnectionResponse' smart constructor.
data CreateVPNConnectionResponse = CreateVPNConnectionResponse'
  { vpnConnection ::
      Lude.Maybe VPNConnection,
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

-- | Creates a value of 'CreateVPNConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpnConnection' - Information about the VPN connection.
mkCreateVPNConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPNConnectionResponse
mkCreateVPNConnectionResponse pResponseStatus_ =
  CreateVPNConnectionResponse'
    { vpnConnection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPN connection.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrsVPNConnection :: Lens.Lens' CreateVPNConnectionResponse (Lude.Maybe VPNConnection)
cvcrsVPNConnection = Lens.lens (vpnConnection :: CreateVPNConnectionResponse -> Lude.Maybe VPNConnection) (\s a -> s {vpnConnection = a} :: CreateVPNConnectionResponse)
{-# DEPRECATED cvcrsVPNConnection "Use generic-lens or generic-optics with 'vpnConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrsResponseStatus :: Lens.Lens' CreateVPNConnectionResponse Lude.Int
cvcrsResponseStatus = Lens.lens (responseStatus :: CreateVPNConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPNConnectionResponse)
{-# DEPRECATED cvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
