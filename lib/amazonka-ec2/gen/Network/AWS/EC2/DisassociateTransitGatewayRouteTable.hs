{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
  ( -- * Creating a request
    DisassociateTransitGatewayRouteTable (..),
    mkDisassociateTransitGatewayRouteTable,

    -- ** Request lenses
    dtgrttDryRun,
    dtgrttTransitGatewayRouteTableId,
    dtgrttTransitGatewayAttachmentId,

    -- * Destructuring the response
    DisassociateTransitGatewayRouteTableResponse (..),
    mkDisassociateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    dtgrttrsAssociation,
    dtgrttrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
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

-- | Creates a value of 'DisassociateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkDisassociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  DisassociateTransitGatewayRouteTable
mkDisassociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    DisassociateTransitGatewayRouteTable'
      { dryRun = Lude.Nothing,
        transitGatewayRouteTableId = pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId = pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrttDryRun :: Lens.Lens' DisassociateTransitGatewayRouteTable (Lude.Maybe Lude.Bool)
dtgrttDryRun = Lens.lens (dryRun :: DisassociateTransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateTransitGatewayRouteTable)
{-# DEPRECATED dtgrttDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrttTransitGatewayRouteTableId :: Lens.Lens' DisassociateTransitGatewayRouteTable Lude.Text
dtgrttTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: DisassociateTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: DisassociateTransitGatewayRouteTable)
{-# DEPRECATED dtgrttTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrttTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayRouteTable Lude.Text
dtgrttTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DisassociateTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayRouteTable)
{-# DEPRECATED dtgrttTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest DisassociateTransitGatewayRouteTable where
  type
    Rs DisassociateTransitGatewayRouteTable =
      DisassociateTransitGatewayRouteTableResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayRouteTableResponse'
            Lude.<$> (x Lude..@? "association") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateTransitGatewayRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateTransitGatewayRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateTransitGatewayRouteTable where
  toQuery DisassociateTransitGatewayRouteTable' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateTransitGatewayRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkDisassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { association ::
      Lude.Maybe
        TransitGatewayAssociation,
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

-- | Creates a value of 'DisassociateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- * 'association' - Information about the association.
-- * 'responseStatus' - The response status code.
mkDisassociateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateTransitGatewayRouteTableResponse
mkDisassociateTransitGatewayRouteTableResponse pResponseStatus_ =
  DisassociateTransitGatewayRouteTableResponse'
    { association =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrttrsAssociation :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse (Lude.Maybe TransitGatewayAssociation)
dtgrttrsAssociation = Lens.lens (association :: DisassociateTransitGatewayRouteTableResponse -> Lude.Maybe TransitGatewayAssociation) (\s a -> s {association = a} :: DisassociateTransitGatewayRouteTableResponse)
{-# DEPRECATED dtgrttrsAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrttrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse Lude.Int
dtgrttrsResponseStatus = Lens.lens (responseStatus :: DisassociateTransitGatewayRouteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateTransitGatewayRouteTableResponse)
{-# DEPRECATED dtgrttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
