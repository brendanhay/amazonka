{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateClientVPNRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a route to a network to a Client VPN endpoint. Each Client VPN endpoint has a route table that describes the available destination network routes. Each route in the route table specifies the path for traﬃc to speciﬁc resources or networks.
module Network.AWS.EC2.CreateClientVPNRoute
  ( -- * Creating a request
    CreateClientVPNRoute (..),
    mkCreateClientVPNRoute,

    -- ** Request lenses
    ccvrClientToken,
    ccvrDescription,
    ccvrDryRun,
    ccvrClientVPNEndpointId,
    ccvrDestinationCidrBlock,
    ccvrTargetVPCSubnetId,

    -- * Destructuring the response
    CreateClientVPNRouteResponse (..),
    mkCreateClientVPNRouteResponse,

    -- ** Response lenses
    ccvrrsStatus,
    ccvrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateClientVPNRoute' smart constructor.
data CreateClientVPNRoute = CreateClientVPNRoute'
  { clientToken ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientVPNEndpointId :: Lude.Text,
    destinationCidrBlock :: Lude.Text,
    targetVPCSubnetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClientVPNRoute' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint to which to add the route.
-- * 'description' - A brief description of the route.
-- * 'destinationCidrBlock' - The IPv4 address range, in CIDR notation, of the route destination. For example:
--
--
--     * To add a route for Internet access, enter @0.0.0.0/0@
--
--
--     * To add a route for a peered VPC, enter the peered VPC's IPv4 CIDR range
--
--
--     * To add a route for an on-premises network, enter the AWS Site-to-Site VPN connection's IPv4 CIDR range
--
--
--     * To add a route for the local network, enter the client CIDR range
--
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'targetVPCSubnetId' - The ID of the subnet through which you want to route traffic. The specified subnet must be an existing target network of the Client VPN endpoint.
--
-- Alternatively, if you're adding a route for the local network, specify @local@ .
mkCreateClientVPNRoute ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  -- | 'targetVPCSubnetId'
  Lude.Text ->
  CreateClientVPNRoute
mkCreateClientVPNRoute
  pClientVPNEndpointId_
  pDestinationCidrBlock_
  pTargetVPCSubnetId_ =
    CreateClientVPNRoute'
      { clientToken = Lude.Nothing,
        description = Lude.Nothing,
        dryRun = Lude.Nothing,
        clientVPNEndpointId = pClientVPNEndpointId_,
        destinationCidrBlock = pDestinationCidrBlock_,
        targetVPCSubnetId = pTargetVPCSubnetId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrClientToken :: Lens.Lens' CreateClientVPNRoute (Lude.Maybe Lude.Text)
ccvrClientToken = Lens.lens (clientToken :: CreateClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A brief description of the route.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDescription :: Lens.Lens' CreateClientVPNRoute (Lude.Maybe Lude.Text)
ccvrDescription = Lens.lens (description :: CreateClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDryRun :: Lens.Lens' CreateClientVPNRoute (Lude.Maybe Lude.Bool)
ccvrDryRun = Lens.lens (dryRun :: CreateClientVPNRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint to which to add the route.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrClientVPNEndpointId :: Lens.Lens' CreateClientVPNRoute Lude.Text
ccvrClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: CreateClientVPNRoute -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the route destination. For example:
--
--
--     * To add a route for Internet access, enter @0.0.0.0/0@
--
--
--     * To add a route for a peered VPC, enter the peered VPC's IPv4 CIDR range
--
--
--     * To add a route for an on-premises network, enter the AWS Site-to-Site VPN connection's IPv4 CIDR range
--
--
--     * To add a route for the local network, enter the client CIDR range
--
--
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDestinationCidrBlock :: Lens.Lens' CreateClientVPNRoute Lude.Text
ccvrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateClientVPNRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the subnet through which you want to route traffic. The specified subnet must be an existing target network of the Client VPN endpoint.
--
-- Alternatively, if you're adding a route for the local network, specify @local@ .
--
-- /Note:/ Consider using 'targetVPCSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrTargetVPCSubnetId :: Lens.Lens' CreateClientVPNRoute Lude.Text
ccvrTargetVPCSubnetId = Lens.lens (targetVPCSubnetId :: CreateClientVPNRoute -> Lude.Text) (\s a -> s {targetVPCSubnetId = a} :: CreateClientVPNRoute)
{-# DEPRECATED ccvrTargetVPCSubnetId "Use generic-lens or generic-optics with 'targetVPCSubnetId' instead." #-}

instance Lude.AWSRequest CreateClientVPNRoute where
  type Rs CreateClientVPNRoute = CreateClientVPNRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateClientVPNRouteResponse'
            Lude.<$> (x Lude..@? "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClientVPNRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateClientVPNRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClientVPNRoute where
  toQuery CreateClientVPNRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateClientVpnRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "TargetVpcSubnetId" Lude.=: targetVPCSubnetId
      ]

-- | /See:/ 'mkCreateClientVPNRouteResponse' smart constructor.
data CreateClientVPNRouteResponse = CreateClientVPNRouteResponse'
  { status ::
      Lude.Maybe ClientVPNRouteStatus,
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

-- | Creates a value of 'CreateClientVPNRouteResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The current state of the route.
mkCreateClientVPNRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClientVPNRouteResponse
mkCreateClientVPNRouteResponse pResponseStatus_ =
  CreateClientVPNRouteResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrrsStatus :: Lens.Lens' CreateClientVPNRouteResponse (Lude.Maybe ClientVPNRouteStatus)
ccvrrsStatus = Lens.lens (status :: CreateClientVPNRouteResponse -> Lude.Maybe ClientVPNRouteStatus) (\s a -> s {status = a} :: CreateClientVPNRouteResponse)
{-# DEPRECATED ccvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrrsResponseStatus :: Lens.Lens' CreateClientVPNRouteResponse Lude.Int
ccvrrsResponseStatus = Lens.lens (responseStatus :: CreateClientVPNRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClientVPNRouteResponse)
{-# DEPRECATED ccvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
