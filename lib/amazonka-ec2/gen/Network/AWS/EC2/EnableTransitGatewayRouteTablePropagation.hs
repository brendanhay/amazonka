{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified attachment to propagate routes to the specified propagation route table.
module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
  ( -- * Creating a request
    EnableTransitGatewayRouteTablePropagation (..),
    mkEnableTransitGatewayRouteTablePropagation,

    -- ** Request lenses
    etgrtpTransitGatewayRouteTableId,
    etgrtpTransitGatewayAttachmentId,
    etgrtpDryRun,

    -- * Destructuring the response
    EnableTransitGatewayRouteTablePropagationResponse (..),
    mkEnableTransitGatewayRouteTablePropagationResponse,

    -- ** Response lenses
    etgrtprsPropagation,
    etgrtprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableTransitGatewayRouteTablePropagation' smart constructor.
data EnableTransitGatewayRouteTablePropagation = EnableTransitGatewayRouteTablePropagation'
  { -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableTransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the propagation route table.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkEnableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  EnableTransitGatewayRouteTablePropagation
mkEnableTransitGatewayRouteTablePropagation
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    EnableTransitGatewayRouteTablePropagation'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_,
        dryRun = Lude.Nothing
      }

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpTransitGatewayRouteTableId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Lude.Text
etgrtpTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: EnableTransitGatewayRouteTablePropagation -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: EnableTransitGatewayRouteTablePropagation)
{-# DEPRECATED etgrtpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpTransitGatewayAttachmentId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Lude.Text
etgrtpTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: EnableTransitGatewayRouteTablePropagation -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: EnableTransitGatewayRouteTablePropagation)
{-# DEPRECATED etgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpDryRun :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Lude.Maybe Lude.Bool)
etgrtpDryRun = Lens.lens (dryRun :: EnableTransitGatewayRouteTablePropagation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableTransitGatewayRouteTablePropagation)
{-# DEPRECATED etgrtpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest EnableTransitGatewayRouteTablePropagation where
  type
    Rs EnableTransitGatewayRouteTablePropagation =
      EnableTransitGatewayRouteTablePropagationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          EnableTransitGatewayRouteTablePropagationResponse'
            Lude.<$> (x Lude..@? "propagation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableTransitGatewayRouteTablePropagation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableTransitGatewayRouteTablePropagation where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableTransitGatewayRouteTablePropagation where
  toQuery EnableTransitGatewayRouteTablePropagation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("EnableTransitGatewayRouteTablePropagation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkEnableTransitGatewayRouteTablePropagationResponse' smart constructor.
data EnableTransitGatewayRouteTablePropagationResponse = EnableTransitGatewayRouteTablePropagationResponse'
  { -- | Information about route propagation.
    propagation :: Lude.Maybe TransitGatewayPropagation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableTransitGatewayRouteTablePropagationResponse' with the minimum fields required to make a request.
--
-- * 'propagation' - Information about route propagation.
-- * 'responseStatus' - The response status code.
mkEnableTransitGatewayRouteTablePropagationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableTransitGatewayRouteTablePropagationResponse
mkEnableTransitGatewayRouteTablePropagationResponse
  pResponseStatus_ =
    EnableTransitGatewayRouteTablePropagationResponse'
      { propagation =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about route propagation.
--
-- /Note:/ Consider using 'propagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtprsPropagation :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse (Lude.Maybe TransitGatewayPropagation)
etgrtprsPropagation = Lens.lens (propagation :: EnableTransitGatewayRouteTablePropagationResponse -> Lude.Maybe TransitGatewayPropagation) (\s a -> s {propagation = a} :: EnableTransitGatewayRouteTablePropagationResponse)
{-# DEPRECATED etgrtprsPropagation "Use generic-lens or generic-optics with 'propagation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtprsResponseStatus :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse Lude.Int
etgrtprsResponseStatus = Lens.lens (responseStatus :: EnableTransitGatewayRouteTablePropagationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableTransitGatewayRouteTablePropagationResponse)
{-# DEPRECATED etgrtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
