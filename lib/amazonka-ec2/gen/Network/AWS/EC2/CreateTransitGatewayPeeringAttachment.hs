{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a transit gateway peering attachment between the specified transit gateway (requester) and a peer transit gateway (accepter). The transit gateways must be in different Regions. The peer transit gateway can be in your account or a different AWS account.
--
-- After you create the peering attachment, the owner of the accepter transit gateway must accept the attachment request.
module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
  ( -- * Creating a request
    CreateTransitGatewayPeeringAttachment (..),
    mkCreateTransitGatewayPeeringAttachment,

    -- ** Request lenses
    ctgpaTransitGatewayId,
    ctgpaPeerTransitGatewayId,
    ctgpaPeerAccountId,
    ctgpaPeerRegion,
    ctgpaDryRun,
    ctgpaTagSpecifications,

    -- * Destructuring the response
    CreateTransitGatewayPeeringAttachmentResponse (..),
    mkCreateTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    ctgparrsTransitGatewayPeeringAttachment,
    ctgparrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Types.TransitGatewayId,
    -- | The ID of the peer transit gateway with which to create the peering attachment.
    peerTransitGatewayId :: Types.TransitAssociationGatewayId,
    -- | The AWS account ID of the owner of the peer transit gateway.
    peerAccountId :: Types.String,
    -- | The Region where the peer transit gateway is located.
    peerRegion :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the transit gateway peering attachment.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkCreateTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayId'
  Types.TransitGatewayId ->
  -- | 'peerTransitGatewayId'
  Types.TransitAssociationGatewayId ->
  -- | 'peerAccountId'
  Types.String ->
  -- | 'peerRegion'
  Types.String ->
  CreateTransitGatewayPeeringAttachment
mkCreateTransitGatewayPeeringAttachment
  transitGatewayId
  peerTransitGatewayId
  peerAccountId
  peerRegion =
    CreateTransitGatewayPeeringAttachment'
      { transitGatewayId,
        peerTransitGatewayId,
        peerAccountId,
        peerRegion,
        dryRun = Core.Nothing,
        tagSpecifications = Core.Nothing
      }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.TransitGatewayId
ctgpaTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED ctgpaTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of the peer transit gateway with which to create the peering attachment.
--
-- /Note:/ Consider using 'peerTransitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.TransitAssociationGatewayId
ctgpaPeerTransitGatewayId = Lens.field @"peerTransitGatewayId"
{-# DEPRECATED ctgpaPeerTransitGatewayId "Use generic-lens or generic-optics with 'peerTransitGatewayId' instead." #-}

-- | The AWS account ID of the owner of the peer transit gateway.
--
-- /Note:/ Consider using 'peerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.String
ctgpaPeerAccountId = Lens.field @"peerAccountId"
{-# DEPRECATED ctgpaPeerAccountId "Use generic-lens or generic-optics with 'peerAccountId' instead." #-}

-- | The Region where the peer transit gateway is located.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.String
ctgpaPeerRegion = Lens.field @"peerRegion"
{-# DEPRECATED ctgpaPeerRegion "Use generic-lens or generic-optics with 'peerRegion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaDryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
ctgpaDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe [Types.TagSpecification])
ctgpaTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ctgpaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateTransitGatewayPeeringAttachment where
  type
    Rs CreateTransitGatewayPeeringAttachment =
      CreateTransitGatewayPeeringAttachmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateTransitGatewayPeeringAttachment")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TransitGatewayId" transitGatewayId)
                Core.<> (Core.toQueryValue "PeerTransitGatewayId" peerTransitGatewayId)
                Core.<> (Core.toQueryValue "PeerAccountId" peerAccountId)
                Core.<> (Core.toQueryValue "PeerRegion" peerRegion)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPeeringAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayPeeringAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkCreateTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTransitGatewayPeeringAttachmentResponse
mkCreateTransitGatewayPeeringAttachmentResponse responseStatus =
  CreateTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Core.Nothing,
      responseStatus
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparrsTransitGatewayPeeringAttachment :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
ctgparrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# DEPRECATED ctgparrsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparrsResponseStatus :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse Core.Int
ctgparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
