{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
  ( -- * Creating a request
    DeleteTransitGatewayPrefixListReference (..),
    mkDeleteTransitGatewayPrefixListReference,

    -- ** Request lenses
    dtgplrDryRun,
    dtgplrTransitGatewayRouteTableId,
    dtgplrPrefixListId,

    -- * Destructuring the response
    DeleteTransitGatewayPrefixListReferenceResponse (..),
    mkDeleteTransitGatewayPrefixListReferenceResponse,

    -- ** Response lenses
    dtgplrrsTransitGatewayPrefixListReference,
    dtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayPrefixListReference' smart constructor.
data DeleteTransitGatewayPrefixListReference = DeleteTransitGatewayPrefixListReference'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayRouteTableId ::
      Lude.Text,
    prefixListId ::
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

-- | Creates a value of 'DeleteTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'prefixListId' - The ID of the prefix list.
-- * 'transitGatewayRouteTableId' - The ID of the route table.
mkDeleteTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'prefixListId'
  Lude.Text ->
  DeleteTransitGatewayPrefixListReference
mkDeleteTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    DeleteTransitGatewayPrefixListReference'
      { dryRun = Lude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        prefixListId = pPrefixListId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrDryRun :: Lens.Lens' DeleteTransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
dtgplrDryRun = Lens.lens (dryRun :: DeleteTransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayPrefixListReference)
{-# DEPRECATED dtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Lude.Text
dtgplrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: DeleteTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayPrefixListReference)
{-# DEPRECATED dtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrPrefixListId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Lude.Text
dtgplrPrefixListId = Lens.lens (prefixListId :: DeleteTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {prefixListId = a} :: DeleteTransitGatewayPrefixListReference)
{-# DEPRECATED dtgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayPrefixListReference where
  type
    Rs DeleteTransitGatewayPrefixListReference =
      DeleteTransitGatewayPrefixListReferenceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPrefixListReferenceResponse'
            Lude.<$> (x Lude..@? "transitGatewayPrefixListReference")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayPrefixListReference where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayPrefixListReference where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayPrefixListReference where
  toQuery DeleteTransitGatewayPrefixListReference' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTransitGatewayPrefixListReference" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "PrefixListId" Lude.=: prefixListId
      ]

-- | /See:/ 'mkDeleteTransitGatewayPrefixListReferenceResponse' smart constructor.
data DeleteTransitGatewayPrefixListReferenceResponse = DeleteTransitGatewayPrefixListReferenceResponse'
  { transitGatewayPrefixListReference ::
      Lude.Maybe
        TransitGatewayPrefixListReference,
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

-- | Creates a value of 'DeleteTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPrefixListReference' - Information about the deleted prefix list reference.
mkDeleteTransitGatewayPrefixListReferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayPrefixListReferenceResponse
mkDeleteTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  DeleteTransitGatewayPrefixListReferenceResponse'
    { transitGatewayPrefixListReference =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrrsTransitGatewayPrefixListReference :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse (Lude.Maybe TransitGatewayPrefixListReference)
dtgplrrsTransitGatewayPrefixListReference = Lens.lens (transitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> Lude.Maybe TransitGatewayPrefixListReference) (\s a -> s {transitGatewayPrefixListReference = a} :: DeleteTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED dtgplrrsTransitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse Lude.Int
dtgplrrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayPrefixListReferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED dtgplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
