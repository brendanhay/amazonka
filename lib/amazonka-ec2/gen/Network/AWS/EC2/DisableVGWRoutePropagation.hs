{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVGWRoutePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a virtual private gateway (VGW) from propagating routes to a specified route table of a VPC.
module Network.AWS.EC2.DisableVGWRoutePropagation
  ( -- * Creating a request
    DisableVGWRoutePropagation (..),
    mkDisableVGWRoutePropagation,

    -- ** Request lenses
    dvrpRouteTableId,
    dvrpGatewayId,
    dvrpDryRun,

    -- * Destructuring the response
    DisableVGWRoutePropagationResponse (..),
    mkDisableVGWRoutePropagationResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DisableVgwRoutePropagation.
--
-- /See:/ 'mkDisableVGWRoutePropagation' smart constructor.
data DisableVGWRoutePropagation = DisableVGWRoutePropagation'
  { -- | The ID of the route table.
    routeTableId :: Lude.Text,
    -- | The ID of the virtual private gateway.
    gatewayId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableVGWRoutePropagation' with the minimum fields required to make a request.
--
-- * 'routeTableId' - The ID of the route table.
-- * 'gatewayId' - The ID of the virtual private gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisableVGWRoutePropagation ::
  -- | 'routeTableId'
  Lude.Text ->
  -- | 'gatewayId'
  Lude.Text ->
  DisableVGWRoutePropagation
mkDisableVGWRoutePropagation pRouteTableId_ pGatewayId_ =
  DisableVGWRoutePropagation'
    { routeTableId = pRouteTableId_,
      gatewayId = pGatewayId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpRouteTableId :: Lens.Lens' DisableVGWRoutePropagation Lude.Text
dvrpRouteTableId = Lens.lens (routeTableId :: DisableVGWRoutePropagation -> Lude.Text) (\s a -> s {routeTableId = a} :: DisableVGWRoutePropagation)
{-# DEPRECATED dvrpRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpGatewayId :: Lens.Lens' DisableVGWRoutePropagation Lude.Text
dvrpGatewayId = Lens.lens (gatewayId :: DisableVGWRoutePropagation -> Lude.Text) (\s a -> s {gatewayId = a} :: DisableVGWRoutePropagation)
{-# DEPRECATED dvrpGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpDryRun :: Lens.Lens' DisableVGWRoutePropagation (Lude.Maybe Lude.Bool)
dvrpDryRun = Lens.lens (dryRun :: DisableVGWRoutePropagation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisableVGWRoutePropagation)
{-# DEPRECATED dvrpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisableVGWRoutePropagation where
  type
    Rs DisableVGWRoutePropagation =
      DisableVGWRoutePropagationResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DisableVGWRoutePropagationResponse'

instance Lude.ToHeaders DisableVGWRoutePropagation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableVGWRoutePropagation where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableVGWRoutePropagation where
  toQuery DisableVGWRoutePropagation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableVgwRoutePropagation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "RouteTableId" Lude.=: routeTableId,
        "GatewayId" Lude.=: gatewayId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisableVGWRoutePropagationResponse' smart constructor.
data DisableVGWRoutePropagationResponse = DisableVGWRoutePropagationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableVGWRoutePropagationResponse' with the minimum fields required to make a request.
mkDisableVGWRoutePropagationResponse ::
  DisableVGWRoutePropagationResponse
mkDisableVGWRoutePropagationResponse =
  DisableVGWRoutePropagationResponse'
