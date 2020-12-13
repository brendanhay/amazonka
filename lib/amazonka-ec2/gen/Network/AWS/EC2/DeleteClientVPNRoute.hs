{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteClientVPNRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a route from a Client VPN endpoint. You can only delete routes that you manually added using the __CreateClientVpnRoute__ action. You cannot delete routes that were automatically added when associating a subnet. To remove routes that have been automatically added, disassociate the target subnet from the Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVPNRoute
  ( -- * Creating a request
    DeleteClientVPNRoute (..),
    mkDeleteClientVPNRoute,

    -- ** Request lenses
    dcvpnrTargetVPCSubnetId,
    dcvpnrClientVPNEndpointId,
    dcvpnrDryRun,
    dcvpnrDestinationCidrBlock,

    -- * Destructuring the response
    DeleteClientVPNRouteResponse (..),
    mkDeleteClientVPNRouteResponse,

    -- ** Response lenses
    dcvpnrrsStatus,
    dcvpnrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteClientVPNRoute' smart constructor.
data DeleteClientVPNRoute = DeleteClientVPNRoute'
  { -- | The ID of the target subnet used by the route.
    targetVPCSubnetId :: Lude.Maybe Lude.Text,
    -- | The ID of the Client VPN endpoint from which the route is to be deleted.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The IPv4 address range, in CIDR notation, of the route to be deleted.
    destinationCidrBlock :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClientVPNRoute' with the minimum fields required to make a request.
--
-- * 'targetVPCSubnetId' - The ID of the target subnet used by the route.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint from which the route is to be deleted.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'destinationCidrBlock' - The IPv4 address range, in CIDR notation, of the route to be deleted.
mkDeleteClientVPNRoute ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  DeleteClientVPNRoute
mkDeleteClientVPNRoute pClientVPNEndpointId_ pDestinationCidrBlock_ =
  DeleteClientVPNRoute'
    { targetVPCSubnetId = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_,
      dryRun = Lude.Nothing,
      destinationCidrBlock = pDestinationCidrBlock_
    }

-- | The ID of the target subnet used by the route.
--
-- /Note:/ Consider using 'targetVPCSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrTargetVPCSubnetId :: Lens.Lens' DeleteClientVPNRoute (Lude.Maybe Lude.Text)
dcvpnrTargetVPCSubnetId = Lens.lens (targetVPCSubnetId :: DeleteClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {targetVPCSubnetId = a} :: DeleteClientVPNRoute)
{-# DEPRECATED dcvpnrTargetVPCSubnetId "Use generic-lens or generic-optics with 'targetVPCSubnetId' instead." #-}

-- | The ID of the Client VPN endpoint from which the route is to be deleted.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrClientVPNEndpointId :: Lens.Lens' DeleteClientVPNRoute Lude.Text
dcvpnrClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DeleteClientVPNRoute -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DeleteClientVPNRoute)
{-# DEPRECATED dcvpnrClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrDryRun :: Lens.Lens' DeleteClientVPNRoute (Lude.Maybe Lude.Bool)
dcvpnrDryRun = Lens.lens (dryRun :: DeleteClientVPNRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteClientVPNRoute)
{-# DEPRECATED dcvpnrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the route to be deleted.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrDestinationCidrBlock :: Lens.Lens' DeleteClientVPNRoute Lude.Text
dcvpnrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: DeleteClientVPNRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: DeleteClientVPNRoute)
{-# DEPRECATED dcvpnrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest DeleteClientVPNRoute where
  type Rs DeleteClientVPNRoute = DeleteClientVPNRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteClientVPNRouteResponse'
            Lude.<$> (x Lude..@? "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteClientVPNRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClientVPNRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClientVPNRoute where
  toQuery DeleteClientVPNRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteClientVpnRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TargetVpcSubnetId" Lude.=: targetVPCSubnetId,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkDeleteClientVPNRouteResponse' smart constructor.
data DeleteClientVPNRouteResponse = DeleteClientVPNRouteResponse'
  { -- | The current state of the route.
    status :: Lude.Maybe ClientVPNRouteStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClientVPNRouteResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the route.
-- * 'responseStatus' - The response status code.
mkDeleteClientVPNRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteClientVPNRouteResponse
mkDeleteClientVPNRouteResponse pResponseStatus_ =
  DeleteClientVPNRouteResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrrsStatus :: Lens.Lens' DeleteClientVPNRouteResponse (Lude.Maybe ClientVPNRouteStatus)
dcvpnrrsStatus = Lens.lens (status :: DeleteClientVPNRouteResponse -> Lude.Maybe ClientVPNRouteStatus) (\s a -> s {status = a} :: DeleteClientVPNRouteResponse)
{-# DEPRECATED dcvpnrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnrrsResponseStatus :: Lens.Lens' DeleteClientVPNRouteResponse Lude.Int
dcvpnrrsResponseStatus = Lens.lens (responseStatus :: DeleteClientVPNRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClientVPNRouteResponse)
{-# DEPRECATED dcvpnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
