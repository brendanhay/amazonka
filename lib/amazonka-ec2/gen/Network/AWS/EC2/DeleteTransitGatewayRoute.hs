{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayRoute
  ( -- * Creating a request
    DeleteTransitGatewayRoute (..),
    mkDeleteTransitGatewayRoute,

    -- ** Request lenses
    dtgrTransitGatewayRouteTableId,
    dtgrDryRun,
    dtgrDestinationCidrBlock,

    -- * Destructuring the response
    DeleteTransitGatewayRouteResponse (..),
    mkDeleteTransitGatewayRouteResponse,

    -- ** Response lenses
    dtgrrsRoute,
    dtgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayRoute' smart constructor.
data DeleteTransitGatewayRoute = DeleteTransitGatewayRoute'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The CIDR range for the route. This must match the CIDR for the route exactly.
    destinationCidrBlock :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayRoute' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'destinationCidrBlock' - The CIDR range for the route. This must match the CIDR for the route exactly.
mkDeleteTransitGatewayRoute ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  DeleteTransitGatewayRoute
mkDeleteTransitGatewayRoute
  pTransitGatewayRouteTableId_
  pDestinationCidrBlock_ =
    DeleteTransitGatewayRoute'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        dryRun = Lude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRoute Lude.Text
dtgrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: DeleteTransitGatewayRoute -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayRoute)
{-# DEPRECATED dtgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDryRun :: Lens.Lens' DeleteTransitGatewayRoute (Lude.Maybe Lude.Bool)
dtgrDryRun = Lens.lens (dryRun :: DeleteTransitGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayRoute)
{-# DEPRECATED dtgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDestinationCidrBlock :: Lens.Lens' DeleteTransitGatewayRoute Lude.Text
dtgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: DeleteTransitGatewayRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: DeleteTransitGatewayRoute)
{-# DEPRECATED dtgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayRoute where
  type
    Rs DeleteTransitGatewayRoute =
      DeleteTransitGatewayRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteResponse'
            Lude.<$> (x Lude..@? "route") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayRoute where
  toQuery DeleteTransitGatewayRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTransitGatewayRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkDeleteTransitGatewayRouteResponse' smart constructor.
data DeleteTransitGatewayRouteResponse = DeleteTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Lude.Maybe TransitGatewayRoute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayRouteResponse' with the minimum fields required to make a request.
--
-- * 'route' - Information about the route.
-- * 'responseStatus' - The response status code.
mkDeleteTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayRouteResponse
mkDeleteTransitGatewayRouteResponse pResponseStatus_ =
  DeleteTransitGatewayRouteResponse'
    { route = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsRoute :: Lens.Lens' DeleteTransitGatewayRouteResponse (Lude.Maybe TransitGatewayRoute)
dtgrrsRoute = Lens.lens (route :: DeleteTransitGatewayRouteResponse -> Lude.Maybe TransitGatewayRoute) (\s a -> s {route = a} :: DeleteTransitGatewayRouteResponse)
{-# DEPRECATED dtgrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteResponse Lude.Int
dtgrrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayRouteResponse)
{-# DEPRECATED dtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
