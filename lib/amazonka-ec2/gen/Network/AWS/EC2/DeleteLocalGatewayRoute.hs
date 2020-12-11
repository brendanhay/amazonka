{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRoute
  ( -- * Creating a request
    DeleteLocalGatewayRoute (..),
    mkDeleteLocalGatewayRoute,

    -- ** Request lenses
    dlgrDryRun,
    dlgrDestinationCidrBlock,
    dlgrLocalGatewayRouteTableId,

    -- * Destructuring the response
    DeleteLocalGatewayRouteResponse (..),
    mkDeleteLocalGatewayRouteResponse,

    -- ** Response lenses
    dlgrrsRoute,
    dlgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLocalGatewayRoute' smart constructor.
data DeleteLocalGatewayRoute = DeleteLocalGatewayRoute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    destinationCidrBlock :: Lude.Text,
    localGatewayRouteTableId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLocalGatewayRoute' with the minimum fields required to make a request.
--
-- * 'destinationCidrBlock' - The CIDR range for the route. This must match the CIDR for the route exactly.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
mkDeleteLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Lude.Text ->
  -- | 'localGatewayRouteTableId'
  Lude.Text ->
  DeleteLocalGatewayRoute
mkDeleteLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_ =
    DeleteLocalGatewayRoute'
      { dryRun = Lude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        localGatewayRouteTableId = pLocalGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDryRun :: Lens.Lens' DeleteLocalGatewayRoute (Lude.Maybe Lude.Bool)
dlgrDryRun = Lens.lens (dryRun :: DeleteLocalGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteLocalGatewayRoute)
{-# DEPRECATED dlgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDestinationCidrBlock :: Lens.Lens' DeleteLocalGatewayRoute Lude.Text
dlgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: DeleteLocalGatewayRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: DeleteLocalGatewayRoute)
{-# DEPRECATED dlgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrLocalGatewayRouteTableId :: Lens.Lens' DeleteLocalGatewayRoute Lude.Text
dlgrLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: DeleteLocalGatewayRoute -> Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: DeleteLocalGatewayRoute)
{-# DEPRECATED dlgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

instance Lude.AWSRequest DeleteLocalGatewayRoute where
  type Rs DeleteLocalGatewayRoute = DeleteLocalGatewayRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteResponse'
            Lude.<$> (x Lude..@? "route") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLocalGatewayRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLocalGatewayRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLocalGatewayRoute where
  toQuery DeleteLocalGatewayRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLocalGatewayRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "LocalGatewayRouteTableId" Lude.=: localGatewayRouteTableId
      ]

-- | /See:/ 'mkDeleteLocalGatewayRouteResponse' smart constructor.
data DeleteLocalGatewayRouteResponse = DeleteLocalGatewayRouteResponse'
  { route ::
      Lude.Maybe
        LocalGatewayRoute,
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

-- | Creates a value of 'DeleteLocalGatewayRouteResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'route' - Information about the route.
mkDeleteLocalGatewayRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLocalGatewayRouteResponse
mkDeleteLocalGatewayRouteResponse pResponseStatus_ =
  DeleteLocalGatewayRouteResponse'
    { route = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsRoute :: Lens.Lens' DeleteLocalGatewayRouteResponse (Lude.Maybe LocalGatewayRoute)
dlgrrsRoute = Lens.lens (route :: DeleteLocalGatewayRouteResponse -> Lude.Maybe LocalGatewayRoute) (\s a -> s {route = a} :: DeleteLocalGatewayRouteResponse)
{-# DEPRECATED dlgrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsResponseStatus :: Lens.Lens' DeleteLocalGatewayRouteResponse Lude.Int
dlgrrsResponseStatus = Lens.lens (responseStatus :: DeleteLocalGatewayRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLocalGatewayRouteResponse)
{-# DEPRECATED dlgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
