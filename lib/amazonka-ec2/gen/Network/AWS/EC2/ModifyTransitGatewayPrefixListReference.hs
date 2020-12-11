{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
  ( -- * Creating a request
    ModifyTransitGatewayPrefixListReference (..),
    mkModifyTransitGatewayPrefixListReference,

    -- ** Request lenses
    mtgplrBlackhole,
    mtgplrTransitGatewayAttachmentId,
    mtgplrDryRun,
    mtgplrTransitGatewayRouteTableId,
    mtgplrPrefixListId,

    -- * Destructuring the response
    ModifyTransitGatewayPrefixListReferenceResponse (..),
    mkModifyTransitGatewayPrefixListReferenceResponse,

    -- ** Response lenses
    mtgplrrsTransitGatewayPrefixListReference,
    mtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { blackhole ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayAttachmentId ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
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

-- | Creates a value of 'ModifyTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- * 'blackhole' - Indicates whether to drop traffic that matches this route.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'prefixListId' - The ID of the prefix list.
-- * 'transitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkModifyTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'prefixListId'
  Lude.Text ->
  ModifyTransitGatewayPrefixListReference
mkModifyTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    ModifyTransitGatewayPrefixListReference'
      { blackhole =
          Lude.Nothing,
        transitGatewayAttachmentId = Lude.Nothing,
        dryRun = Lude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        prefixListId = pPrefixListId_
      }

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrBlackhole :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
mtgplrBlackhole = Lens.lens (blackhole :: ModifyTransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {blackhole = a} :: ModifyTransitGatewayPrefixListReference)
{-# DEPRECATED mtgplrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the attachment to which traffic is routed.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Lude.Maybe Lude.Text)
mtgplrTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: ModifyTransitGatewayPrefixListReference -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: ModifyTransitGatewayPrefixListReference)
{-# DEPRECATED mtgplrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrDryRun :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
mtgplrDryRun = Lens.lens (dryRun :: ModifyTransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTransitGatewayPrefixListReference)
{-# DEPRECATED mtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayRouteTableId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Lude.Text
mtgplrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: ModifyTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: ModifyTransitGatewayPrefixListReference)
{-# DEPRECATED mtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrPrefixListId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Lude.Text
mtgplrPrefixListId = Lens.lens (prefixListId :: ModifyTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {prefixListId = a} :: ModifyTransitGatewayPrefixListReference)
{-# DEPRECATED mtgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

instance Lude.AWSRequest ModifyTransitGatewayPrefixListReference where
  type
    Rs ModifyTransitGatewayPrefixListReference =
      ModifyTransitGatewayPrefixListReferenceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTransitGatewayPrefixListReferenceResponse'
            Lude.<$> (x Lude..@? "transitGatewayPrefixListReference")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTransitGatewayPrefixListReference where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTransitGatewayPrefixListReference where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTransitGatewayPrefixListReference where
  toQuery ModifyTransitGatewayPrefixListReference' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTransitGatewayPrefixListReference" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Blackhole" Lude.=: blackhole,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "PrefixListId" Lude.=: prefixListId
      ]

-- | /See:/ 'mkModifyTransitGatewayPrefixListReferenceResponse' smart constructor.
data ModifyTransitGatewayPrefixListReferenceResponse = ModifyTransitGatewayPrefixListReferenceResponse'
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

-- | Creates a value of 'ModifyTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPrefixListReference' - Information about the prefix list reference.
mkModifyTransitGatewayPrefixListReferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTransitGatewayPrefixListReferenceResponse
mkModifyTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  ModifyTransitGatewayPrefixListReferenceResponse'
    { transitGatewayPrefixListReference =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrsTransitGatewayPrefixListReference :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse (Lude.Maybe TransitGatewayPrefixListReference)
mtgplrrsTransitGatewayPrefixListReference = Lens.lens (transitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> Lude.Maybe TransitGatewayPrefixListReference) (\s a -> s {transitGatewayPrefixListReference = a} :: ModifyTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED mtgplrrsTransitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrsResponseStatus :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse Lude.Int
mtgplrrsResponseStatus = Lens.lens (responseStatus :: ModifyTransitGatewayPrefixListReferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED mtgplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
