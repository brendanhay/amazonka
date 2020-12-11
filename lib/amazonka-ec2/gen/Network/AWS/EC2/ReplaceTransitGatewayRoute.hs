{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified route in the specified transit gateway route table.
module Network.AWS.EC2.ReplaceTransitGatewayRoute
  ( -- * Creating a request
    ReplaceTransitGatewayRoute (..),
    mkReplaceTransitGatewayRoute,

    -- ** Request lenses
    rtgrBlackhole,
    rtgrTransitGatewayAttachmentId,
    rtgrDryRun,
    rtgrDestinationCidrBlock,
    rtgrTransitGatewayRouteTableId,

    -- * Destructuring the response
    ReplaceTransitGatewayRouteResponse (..),
    mkReplaceTransitGatewayRouteResponse,

    -- ** Response lenses
    rtgrrsRoute,
    rtgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceTransitGatewayRoute' smart constructor.
data ReplaceTransitGatewayRoute = ReplaceTransitGatewayRoute'
  { blackhole ::
      Lude.Maybe Lude.Bool,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    destinationCidrBlock :: Lude.Text,
    transitGatewayRouteTableId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceTransitGatewayRoute' with the minimum fields required to make a request.
--
-- * 'blackhole' - Indicates whether traffic matching this route is to be dropped.
-- * 'destinationCidrBlock' - The CIDR range used for the destination match. Routing decisions are based on the most specific match.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayRouteTableId' - The ID of the route table.
mkReplaceTransitGatewayRoute ::
  -- | 'destinationCidrBlock'
  Lude.Text ->
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  ReplaceTransitGatewayRoute
mkReplaceTransitGatewayRoute
  pDestinationCidrBlock_
  pTransitGatewayRouteTableId_ =
    ReplaceTransitGatewayRoute'
      { blackhole = Lude.Nothing,
        transitGatewayAttachmentId = Lude.Nothing,
        dryRun = Lude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        transitGatewayRouteTableId = pTransitGatewayRouteTableId_
      }

-- | Indicates whether traffic matching this route is to be dropped.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrBlackhole :: Lens.Lens' ReplaceTransitGatewayRoute (Lude.Maybe Lude.Bool)
rtgrBlackhole = Lens.lens (blackhole :: ReplaceTransitGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {blackhole = a} :: ReplaceTransitGatewayRoute)
{-# DEPRECATED rtgrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayAttachmentId :: Lens.Lens' ReplaceTransitGatewayRoute (Lude.Maybe Lude.Text)
rtgrTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: ReplaceTransitGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: ReplaceTransitGatewayRoute)
{-# DEPRECATED rtgrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDryRun :: Lens.Lens' ReplaceTransitGatewayRoute (Lude.Maybe Lude.Bool)
rtgrDryRun = Lens.lens (dryRun :: ReplaceTransitGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReplaceTransitGatewayRoute)
{-# DEPRECATED rtgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The CIDR range used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDestinationCidrBlock :: Lens.Lens' ReplaceTransitGatewayRoute Lude.Text
rtgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: ReplaceTransitGatewayRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: ReplaceTransitGatewayRoute)
{-# DEPRECATED rtgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayRouteTableId :: Lens.Lens' ReplaceTransitGatewayRoute Lude.Text
rtgrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: ReplaceTransitGatewayRoute -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: ReplaceTransitGatewayRoute)
{-# DEPRECATED rtgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Lude.AWSRequest ReplaceTransitGatewayRoute where
  type
    Rs ReplaceTransitGatewayRoute =
      ReplaceTransitGatewayRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ReplaceTransitGatewayRouteResponse'
            Lude.<$> (x Lude..@? "route") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReplaceTransitGatewayRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceTransitGatewayRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceTransitGatewayRoute where
  toQuery ReplaceTransitGatewayRoute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ReplaceTransitGatewayRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Blackhole" Lude.=: blackhole,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'mkReplaceTransitGatewayRouteResponse' smart constructor.
data ReplaceTransitGatewayRouteResponse = ReplaceTransitGatewayRouteResponse'
  { route ::
      Lude.Maybe
        TransitGatewayRoute,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceTransitGatewayRouteResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'route' - Information about the modified route.
mkReplaceTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReplaceTransitGatewayRouteResponse
mkReplaceTransitGatewayRouteResponse pResponseStatus_ =
  ReplaceTransitGatewayRouteResponse'
    { route = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the modified route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrsRoute :: Lens.Lens' ReplaceTransitGatewayRouteResponse (Lude.Maybe TransitGatewayRoute)
rtgrrsRoute = Lens.lens (route :: ReplaceTransitGatewayRouteResponse -> Lude.Maybe TransitGatewayRoute) (\s a -> s {route = a} :: ReplaceTransitGatewayRouteResponse)
{-# DEPRECATED rtgrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrsResponseStatus :: Lens.Lens' ReplaceTransitGatewayRouteResponse Lude.Int
rtgrrsResponseStatus = Lens.lens (responseStatus :: ReplaceTransitGatewayRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReplaceTransitGatewayRouteResponse)
{-# DEPRECATED rtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
