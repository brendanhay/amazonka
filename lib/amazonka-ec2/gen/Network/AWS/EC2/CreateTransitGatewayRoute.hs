{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayRoute
  ( -- * Creating a request
    CreateTransitGatewayRoute (..),
    mkCreateTransitGatewayRoute,

    -- ** Request lenses
    ctgrTransitGatewayRouteTableId,
    ctgrBlackhole,
    ctgrTransitGatewayAttachmentId,
    ctgrDryRun,
    ctgrDestinationCidrBlock,

    -- * Destructuring the response
    CreateTransitGatewayRouteResponse (..),
    mkCreateTransitGatewayRouteResponse,

    -- ** Response lenses
    ctgrrsRoute,
    ctgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayRoute' smart constructor.
data CreateTransitGatewayRoute = CreateTransitGatewayRoute'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Lude.Maybe Lude.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
    destinationCidrBlock :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayRoute' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'blackhole' - Indicates whether to drop traffic that matches this route.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'destinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based on the most specific match.
mkCreateTransitGatewayRoute ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  CreateTransitGatewayRoute
mkCreateTransitGatewayRoute
  pTransitGatewayRouteTableId_
  pDestinationCidrBlock_ =
    CreateTransitGatewayRoute'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        blackhole = Lude.Nothing,
        transitGatewayAttachmentId = Lude.Nothing,
        dryRun = Lude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrTransitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayRoute Lude.Text
ctgrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: CreateTransitGatewayRoute -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayRoute)
{-# DEPRECATED ctgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrBlackhole :: Lens.Lens' CreateTransitGatewayRoute (Lude.Maybe Lude.Bool)
ctgrBlackhole = Lens.lens (blackhole :: CreateTransitGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {blackhole = a} :: CreateTransitGatewayRoute)
{-# DEPRECATED ctgrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrTransitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayRoute (Lude.Maybe Lude.Text)
ctgrTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: CreateTransitGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayRoute)
{-# DEPRECATED ctgrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrDryRun :: Lens.Lens' CreateTransitGatewayRoute (Lude.Maybe Lude.Bool)
ctgrDryRun = Lens.lens (dryRun :: CreateTransitGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayRoute)
{-# DEPRECATED ctgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrDestinationCidrBlock :: Lens.Lens' CreateTransitGatewayRoute Lude.Text
ctgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateTransitGatewayRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateTransitGatewayRoute)
{-# DEPRECATED ctgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayRoute where
  type
    Rs CreateTransitGatewayRoute =
      CreateTransitGatewayRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteResponse'
            Lude.<$> (x Lude..@? "route") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayRoute where
  toQuery CreateTransitGatewayRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTransitGatewayRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "Blackhole" Lude.=: blackhole,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkCreateTransitGatewayRouteResponse' smart constructor.
data CreateTransitGatewayRouteResponse = CreateTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Lude.Maybe TransitGatewayRoute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayRouteResponse' with the minimum fields required to make a request.
--
-- * 'route' - Information about the route.
-- * 'responseStatus' - The response status code.
mkCreateTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayRouteResponse
mkCreateTransitGatewayRouteResponse pResponseStatus_ =
  CreateTransitGatewayRouteResponse'
    { route = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsRoute :: Lens.Lens' CreateTransitGatewayRouteResponse (Lude.Maybe TransitGatewayRoute)
ctgrrsRoute = Lens.lens (route :: CreateTransitGatewayRouteResponse -> Lude.Maybe TransitGatewayRoute) (\s a -> s {route = a} :: CreateTransitGatewayRouteResponse)
{-# DEPRECATED ctgrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateTransitGatewayRouteResponse Lude.Int
ctgrrsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayRouteResponse)
{-# DEPRECATED ctgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
