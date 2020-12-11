{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVGWRoutePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the specified route table of a VPC.
module Network.AWS.EC2.EnableVGWRoutePropagation
  ( -- * Creating a request
    EnableVGWRoutePropagation (..),
    mkEnableVGWRoutePropagation,

    -- ** Request lenses
    evrpDryRun,
    evrpGatewayId,
    evrpRouteTableId,

    -- * Destructuring the response
    EnableVGWRoutePropagationResponse (..),
    mkEnableVGWRoutePropagationResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for EnableVgwRoutePropagation.
--
-- /See:/ 'mkEnableVGWRoutePropagation' smart constructor.
data EnableVGWRoutePropagation = EnableVGWRoutePropagation'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    gatewayId :: Lude.Text,
    routeTableId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableVGWRoutePropagation' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'gatewayId' - The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with.
-- * 'routeTableId' - The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to.
mkEnableVGWRoutePropagation ::
  -- | 'gatewayId'
  Lude.Text ->
  -- | 'routeTableId'
  Lude.Text ->
  EnableVGWRoutePropagation
mkEnableVGWRoutePropagation pGatewayId_ pRouteTableId_ =
  EnableVGWRoutePropagation'
    { dryRun = Lude.Nothing,
      gatewayId = pGatewayId_,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpDryRun :: Lens.Lens' EnableVGWRoutePropagation (Lude.Maybe Lude.Bool)
evrpDryRun = Lens.lens (dryRun :: EnableVGWRoutePropagation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableVGWRoutePropagation)
{-# DEPRECATED evrpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpGatewayId :: Lens.Lens' EnableVGWRoutePropagation Lude.Text
evrpGatewayId = Lens.lens (gatewayId :: EnableVGWRoutePropagation -> Lude.Text) (\s a -> s {gatewayId = a} :: EnableVGWRoutePropagation)
{-# DEPRECATED evrpGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpRouteTableId :: Lens.Lens' EnableVGWRoutePropagation Lude.Text
evrpRouteTableId = Lens.lens (routeTableId :: EnableVGWRoutePropagation -> Lude.Text) (\s a -> s {routeTableId = a} :: EnableVGWRoutePropagation)
{-# DEPRECATED evrpRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest EnableVGWRoutePropagation where
  type
    Rs EnableVGWRoutePropagation =
      EnableVGWRoutePropagationResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull EnableVGWRoutePropagationResponse'

instance Lude.ToHeaders EnableVGWRoutePropagation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableVGWRoutePropagation where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableVGWRoutePropagation where
  toQuery EnableVGWRoutePropagation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableVgwRoutePropagation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "GatewayId" Lude.=: gatewayId,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkEnableVGWRoutePropagationResponse' smart constructor.
data EnableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableVGWRoutePropagationResponse' with the minimum fields required to make a request.
mkEnableVGWRoutePropagationResponse ::
  EnableVGWRoutePropagationResponse
mkEnableVGWRoutePropagationResponse =
  EnableVGWRoutePropagationResponse'
