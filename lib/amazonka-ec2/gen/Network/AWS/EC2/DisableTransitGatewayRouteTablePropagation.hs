{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified resource attachment from propagating routes to the specified propagation route table.
module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
  ( -- * Creating a request
    DisableTransitGatewayRouteTablePropagation (..),
    mkDisableTransitGatewayRouteTablePropagation,

    -- ** Request lenses
    dtgrtpDryRun,
    dtgrtpTransitGatewayRouteTableId,
    dtgrtpTransitGatewayAttachmentId,

    -- * Destructuring the response
    DisableTransitGatewayRouteTablePropagationResponse (..),
    mkDisableTransitGatewayRouteTablePropagationResponse,

    -- ** Response lenses
    dtgrtprsPropagation,
    dtgrtprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagation' smart constructor.
data DisableTransitGatewayRouteTablePropagation = DisableTransitGatewayRouteTablePropagation'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayRouteTableId ::
      Lude.Text,
    transitGatewayAttachmentId ::
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

-- | Creates a value of 'DisableTransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayRouteTableId' - The ID of the propagation route table.
mkDisableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  DisableTransitGatewayRouteTablePropagation
mkDisableTransitGatewayRouteTablePropagation
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    DisableTransitGatewayRouteTablePropagation'
      { dryRun =
          Lude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpDryRun :: Lens.Lens' DisableTransitGatewayRouteTablePropagation (Lude.Maybe Lude.Bool)
dtgrtpDryRun = Lens.lens (dryRun :: DisableTransitGatewayRouteTablePropagation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisableTransitGatewayRouteTablePropagation)
{-# DEPRECATED dtgrtpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayRouteTableId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Lude.Text
dtgrtpTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: DisableTransitGatewayRouteTablePropagation -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: DisableTransitGatewayRouteTablePropagation)
{-# DEPRECATED dtgrtpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayAttachmentId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Lude.Text
dtgrtpTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DisableTransitGatewayRouteTablePropagation -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DisableTransitGatewayRouteTablePropagation)
{-# DEPRECATED dtgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest DisableTransitGatewayRouteTablePropagation where
  type
    Rs DisableTransitGatewayRouteTablePropagation =
      DisableTransitGatewayRouteTablePropagationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisableTransitGatewayRouteTablePropagationResponse'
            Lude.<$> (x Lude..@? "propagation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableTransitGatewayRouteTablePropagation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableTransitGatewayRouteTablePropagation where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableTransitGatewayRouteTablePropagation where
  toQuery DisableTransitGatewayRouteTablePropagation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableTransitGatewayRouteTablePropagation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagationResponse' smart constructor.
data DisableTransitGatewayRouteTablePropagationResponse = DisableTransitGatewayRouteTablePropagationResponse'
  { propagation ::
      Lude.Maybe
        TransitGatewayPropagation,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DisableTransitGatewayRouteTablePropagationResponse' with the minimum fields required to make a request.
--
-- * 'propagation' - Information about route propagation.
-- * 'responseStatus' - The response status code.
mkDisableTransitGatewayRouteTablePropagationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableTransitGatewayRouteTablePropagationResponse
mkDisableTransitGatewayRouteTablePropagationResponse
  pResponseStatus_ =
    DisableTransitGatewayRouteTablePropagationResponse'
      { propagation =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about route propagation.
--
-- /Note:/ Consider using 'propagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprsPropagation :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse (Lude.Maybe TransitGatewayPropagation)
dtgrtprsPropagation = Lens.lens (propagation :: DisableTransitGatewayRouteTablePropagationResponse -> Lude.Maybe TransitGatewayPropagation) (\s a -> s {propagation = a} :: DisableTransitGatewayRouteTablePropagationResponse)
{-# DEPRECATED dtgrtprsPropagation "Use generic-lens or generic-optics with 'propagation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprsResponseStatus :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse Lude.Int
dtgrtprsResponseStatus = Lens.lens (responseStatus :: DisableTransitGatewayRouteTablePropagationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableTransitGatewayRouteTablePropagationResponse)
{-# DEPRECATED dtgrtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
