{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayPrefixListReference
  ( -- * Creating a request
    CreateTransitGatewayPrefixListReference (..),
    mkCreateTransitGatewayPrefixListReference,

    -- ** Request lenses
    ctgplrTransitGatewayRouteTableId,
    ctgplrBlackhole,
    ctgplrPrefixListId,
    ctgplrTransitGatewayAttachmentId,
    ctgplrDryRun,

    -- * Destructuring the response
    CreateTransitGatewayPrefixListReferenceResponse (..),
    mkCreateTransitGatewayPrefixListReferenceResponse,

    -- ** Response lenses
    ctgplrrsTransitGatewayPrefixListReference,
    ctgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayPrefixListReference' smart constructor.
data CreateTransitGatewayPrefixListReference = CreateTransitGatewayPrefixListReference'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Lude.Maybe Lude.Bool,
    -- | The ID of the prefix list that is used for destination matches.
    prefixListId :: Lude.Text,
    -- | The ID of the attachment to which traffic is routed.
    transitGatewayAttachmentId :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'blackhole' - Indicates whether to drop traffic that matches this route.
-- * 'prefixListId' - The ID of the prefix list that is used for destination matches.
-- * 'transitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  -- | 'prefixListId'
  Lude.Text ->
  CreateTransitGatewayPrefixListReference
mkCreateTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    CreateTransitGatewayPrefixListReference'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        blackhole = Lude.Nothing,
        prefixListId = pPrefixListId_,
        transitGatewayAttachmentId = Lude.Nothing,
        dryRun = Lude.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrTransitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayPrefixListReference Lude.Text
ctgplrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: CreateTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayPrefixListReference)
{-# DEPRECATED ctgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrBlackhole :: Lens.Lens' CreateTransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
ctgplrBlackhole = Lens.lens (blackhole :: CreateTransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {blackhole = a} :: CreateTransitGatewayPrefixListReference)
{-# DEPRECATED ctgplrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the prefix list that is used for destination matches.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrPrefixListId :: Lens.Lens' CreateTransitGatewayPrefixListReference Lude.Text
ctgplrPrefixListId = Lens.lens (prefixListId :: CreateTransitGatewayPrefixListReference -> Lude.Text) (\s a -> s {prefixListId = a} :: CreateTransitGatewayPrefixListReference)
{-# DEPRECATED ctgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The ID of the attachment to which traffic is routed.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrTransitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayPrefixListReference (Lude.Maybe Lude.Text)
ctgplrTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: CreateTransitGatewayPrefixListReference -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayPrefixListReference)
{-# DEPRECATED ctgplrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrDryRun :: Lens.Lens' CreateTransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
ctgplrDryRun = Lens.lens (dryRun :: CreateTransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayPrefixListReference)
{-# DEPRECATED ctgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayPrefixListReference where
  type
    Rs CreateTransitGatewayPrefixListReference =
      CreateTransitGatewayPrefixListReferenceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayPrefixListReferenceResponse'
            Lude.<$> (x Lude..@? "transitGatewayPrefixListReference")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayPrefixListReference where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayPrefixListReference where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayPrefixListReference where
  toQuery CreateTransitGatewayPrefixListReference' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTransitGatewayPrefixListReference" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        "Blackhole" Lude.=: blackhole,
        "PrefixListId" Lude.=: prefixListId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTransitGatewayPrefixListReferenceResponse' smart constructor.
data CreateTransitGatewayPrefixListReferenceResponse = CreateTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the prefix list reference.
    transitGatewayPrefixListReference :: Lude.Maybe TransitGatewayPrefixListReference,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayPrefixListReferenceResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayPrefixListReference' - Information about the prefix list reference.
-- * 'responseStatus' - The response status code.
mkCreateTransitGatewayPrefixListReferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayPrefixListReferenceResponse
mkCreateTransitGatewayPrefixListReferenceResponse pResponseStatus_ =
  CreateTransitGatewayPrefixListReferenceResponse'
    { transitGatewayPrefixListReference =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrrsTransitGatewayPrefixListReference :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse (Lude.Maybe TransitGatewayPrefixListReference)
ctgplrrsTransitGatewayPrefixListReference = Lens.lens (transitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> Lude.Maybe TransitGatewayPrefixListReference) (\s a -> s {transitGatewayPrefixListReference = a} :: CreateTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED ctgplrrsTransitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrrsResponseStatus :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse Lude.Int
ctgplrrsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayPrefixListReferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayPrefixListReferenceResponse)
{-# DEPRECATED ctgplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
