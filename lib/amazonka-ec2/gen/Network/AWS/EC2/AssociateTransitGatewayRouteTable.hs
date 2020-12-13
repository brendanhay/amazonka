{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified attachment with the specified transit gateway route table. You can associate only one route table with an attachment.
module Network.AWS.EC2.AssociateTransitGatewayRouteTable
  ( -- * Creating a request
    AssociateTransitGatewayRouteTable (..),
    mkAssociateTransitGatewayRouteTable,

    -- ** Request lenses
    atgrtTransitGatewayRouteTableId,
    atgrtTransitGatewayAttachmentId,
    atgrtDryRun,

    -- * Destructuring the response
    AssociateTransitGatewayRouteTableResponse (..),
    mkAssociateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    atgrtrsAssociation,
    atgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateTransitGatewayRouteTable' smart constructor.
data AssociateTransitGatewayRouteTable = AssociateTransitGatewayRouteTable'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAssociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  AssociateTransitGatewayRouteTable
mkAssociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    AssociateTransitGatewayRouteTable'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId = pTransitGatewayAttachmentId_,
        dryRun = Lude.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtTransitGatewayRouteTableId :: Lens.Lens' AssociateTransitGatewayRouteTable Lude.Text
atgrtTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: AssociateTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: AssociateTransitGatewayRouteTable)
{-# DEPRECATED atgrtTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtTransitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayRouteTable Lude.Text
atgrtTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: AssociateTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayRouteTable)
{-# DEPRECATED atgrtTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtDryRun :: Lens.Lens' AssociateTransitGatewayRouteTable (Lude.Maybe Lude.Bool)
atgrtDryRun = Lens.lens (dryRun :: AssociateTransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateTransitGatewayRouteTable)
{-# DEPRECATED atgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateTransitGatewayRouteTable where
  type
    Rs AssociateTransitGatewayRouteTable =
      AssociateTransitGatewayRouteTableResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateTransitGatewayRouteTableResponse'
            Lude.<$> (x Lude..@? "association") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTransitGatewayRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateTransitGatewayRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateTransitGatewayRouteTable where
  toQuery AssociateTransitGatewayRouteTable' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateTransitGatewayRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAssociateTransitGatewayRouteTableResponse' smart constructor.
data AssociateTransitGatewayRouteTableResponse = AssociateTransitGatewayRouteTableResponse'
  { -- | The ID of the association.
    association :: Lude.Maybe TransitGatewayAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- * 'association' - The ID of the association.
-- * 'responseStatus' - The response status code.
mkAssociateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTransitGatewayRouteTableResponse
mkAssociateTransitGatewayRouteTableResponse pResponseStatus_ =
  AssociateTransitGatewayRouteTableResponse'
    { association =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtrsAssociation :: Lens.Lens' AssociateTransitGatewayRouteTableResponse (Lude.Maybe TransitGatewayAssociation)
atgrtrsAssociation = Lens.lens (association :: AssociateTransitGatewayRouteTableResponse -> Lude.Maybe TransitGatewayAssociation) (\s a -> s {association = a} :: AssociateTransitGatewayRouteTableResponse)
{-# DEPRECATED atgrtrsAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtrsResponseStatus :: Lens.Lens' AssociateTransitGatewayRouteTableResponse Lude.Int
atgrtrsResponseStatus = Lens.lens (responseStatus :: AssociateTransitGatewayRouteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTransitGatewayRouteTableResponse)
{-# DEPRECATED atgrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
