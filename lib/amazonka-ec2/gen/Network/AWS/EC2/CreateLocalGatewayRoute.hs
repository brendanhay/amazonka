{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRoute
  ( -- * Creating a request
    CreateLocalGatewayRoute (..),
    mkCreateLocalGatewayRoute,

    -- ** Request lenses
    clgrDryRun,
    clgrDestinationCidrBlock,
    clgrLocalGatewayRouteTableId,
    clgrLocalGatewayVirtualInterfaceGroupId,

    -- * Destructuring the response
    CreateLocalGatewayRouteResponse (..),
    mkCreateLocalGatewayRouteResponse,

    -- ** Response lenses
    clgrrsRoute,
    clgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLocalGatewayRoute' smart constructor.
data CreateLocalGatewayRoute = CreateLocalGatewayRoute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    destinationCidrBlock :: Lude.Text,
    localGatewayRouteTableId :: Lude.Text,
    localGatewayVirtualInterfaceGroupId ::
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

-- | Creates a value of 'CreateLocalGatewayRoute' with the minimum fields required to make a request.
--
-- * 'destinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based on the most specific match.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
mkCreateLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Lude.Text ->
  -- | 'localGatewayRouteTableId'
  Lude.Text ->
  -- | 'localGatewayVirtualInterfaceGroupId'
  Lude.Text ->
  CreateLocalGatewayRoute
mkCreateLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_
  pLocalGatewayVirtualInterfaceGroupId_ =
    CreateLocalGatewayRoute'
      { dryRun = Lude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        localGatewayRouteTableId = pLocalGatewayRouteTableId_,
        localGatewayVirtualInterfaceGroupId =
          pLocalGatewayVirtualInterfaceGroupId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDryRun :: Lens.Lens' CreateLocalGatewayRoute (Lude.Maybe Lude.Bool)
clgrDryRun = Lens.lens (dryRun :: CreateLocalGatewayRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateLocalGatewayRoute)
{-# DEPRECATED clgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDestinationCidrBlock :: Lens.Lens' CreateLocalGatewayRoute Lude.Text
clgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateLocalGatewayRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateLocalGatewayRoute)
{-# DEPRECATED clgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRoute Lude.Text
clgrLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: CreateLocalGatewayRoute -> Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: CreateLocalGatewayRoute)
{-# DEPRECATED clgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' CreateLocalGatewayRoute Lude.Text
clgrLocalGatewayVirtualInterfaceGroupId = Lens.lens (localGatewayVirtualInterfaceGroupId :: CreateLocalGatewayRoute -> Lude.Text) (\s a -> s {localGatewayVirtualInterfaceGroupId = a} :: CreateLocalGatewayRoute)
{-# DEPRECATED clgrLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

instance Lude.AWSRequest CreateLocalGatewayRoute where
  type Rs CreateLocalGatewayRoute = CreateLocalGatewayRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteResponse'
            Lude.<$> (x Lude..@? "route") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLocalGatewayRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLocalGatewayRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLocalGatewayRoute where
  toQuery CreateLocalGatewayRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLocalGatewayRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "LocalGatewayRouteTableId" Lude.=: localGatewayRouteTableId,
        "LocalGatewayVirtualInterfaceGroupId"
          Lude.=: localGatewayVirtualInterfaceGroupId
      ]

-- | /See:/ 'mkCreateLocalGatewayRouteResponse' smart constructor.
data CreateLocalGatewayRouteResponse = CreateLocalGatewayRouteResponse'
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

-- | Creates a value of 'CreateLocalGatewayRouteResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'route' - Information about the route.
mkCreateLocalGatewayRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLocalGatewayRouteResponse
mkCreateLocalGatewayRouteResponse pResponseStatus_ =
  CreateLocalGatewayRouteResponse'
    { route = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrsRoute :: Lens.Lens' CreateLocalGatewayRouteResponse (Lude.Maybe LocalGatewayRoute)
clgrrsRoute = Lens.lens (route :: CreateLocalGatewayRouteResponse -> Lude.Maybe LocalGatewayRoute) (\s a -> s {route = a} :: CreateLocalGatewayRouteResponse)
{-# DEPRECATED clgrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrsResponseStatus :: Lens.Lens' CreateLocalGatewayRouteResponse Lude.Int
clgrrsResponseStatus = Lens.lens (responseStatus :: CreateLocalGatewayRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLocalGatewayRouteResponse)
{-# DEPRECATED clgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
