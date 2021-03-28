{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTransitGatewayPeeringAttachment (..)
    , mkCreateTransitGatewayPeeringAttachment
    -- ** Request lenses
    , ctgpaTransitGatewayId
    , ctgpaPeerTransitGatewayId
    , ctgpaPeerAccountId
    , ctgpaPeerRegion
    , ctgpaDryRun
    , ctgpaTagSpecifications

    -- * Destructuring the response
    , CreateTransitGatewayPeeringAttachmentResponse (..)
    , mkCreateTransitGatewayPeeringAttachmentResponse
    -- ** Response lenses
    , ctgparrsTransitGatewayPeeringAttachment
    , ctgparrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { transitGatewayId :: Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , peerTransitGatewayId :: Types.TransitAssociationGatewayId
    -- ^ The ID of the peer transit gateway with which to create the peering attachment.
  , peerAccountId :: Core.Text
    -- ^ The AWS account ID of the owner of the peer transit gateway.
  , peerRegion :: Core.Text
    -- ^ The Region where the peer transit gateway is located.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the transit gateway peering attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkCreateTransitGatewayPeeringAttachment
    :: Types.TransitGatewayId -- ^ 'transitGatewayId'
    -> Types.TransitAssociationGatewayId -- ^ 'peerTransitGatewayId'
    -> Core.Text -- ^ 'peerAccountId'
    -> Core.Text -- ^ 'peerRegion'
    -> CreateTransitGatewayPeeringAttachment
mkCreateTransitGatewayPeeringAttachment transitGatewayId
  peerTransitGatewayId peerAccountId peerRegion
  = CreateTransitGatewayPeeringAttachment'{transitGatewayId,
                                           peerTransitGatewayId, peerAccountId, peerRegion,
                                           dryRun = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.TransitGatewayId
ctgpaTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE ctgpaTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the peer transit gateway with which to create the peering attachment.
--
-- /Note:/ Consider using 'peerTransitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Types.TransitAssociationGatewayId
ctgpaPeerTransitGatewayId = Lens.field @"peerTransitGatewayId"
{-# INLINEABLE ctgpaPeerTransitGatewayId #-}
{-# DEPRECATED peerTransitGatewayId "Use generic-lens or generic-optics with 'peerTransitGatewayId' instead"  #-}

-- | The AWS account ID of the owner of the peer transit gateway.
--
-- /Note:/ Consider using 'peerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
ctgpaPeerAccountId = Lens.field @"peerAccountId"
{-# INLINEABLE ctgpaPeerAccountId #-}
{-# DEPRECATED peerAccountId "Use generic-lens or generic-optics with 'peerAccountId' instead"  #-}

-- | The Region where the peer transit gateway is located.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
ctgpaPeerRegion = Lens.field @"peerRegion"
{-# INLINEABLE ctgpaPeerRegion #-}
{-# DEPRECATED peerRegion "Use generic-lens or generic-optics with 'peerRegion' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaDryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
ctgpaDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctgpaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe [Types.TagSpecification])
ctgpaTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctgpaTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTransitGatewayPeeringAttachment where
        toQuery CreateTransitGatewayPeeringAttachment{..}
          = Core.toQueryPair "Action"
              ("CreateTransitGatewayPeeringAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "TransitGatewayId" transitGatewayId
              Core.<>
              Core.toQueryPair "PeerTransitGatewayId" peerTransitGatewayId
              Core.<> Core.toQueryPair "PeerAccountId" peerAccountId
              Core.<> Core.toQueryPair "PeerRegion" peerRegion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateTransitGatewayPeeringAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTransitGatewayPeeringAttachment
         where
        type Rs CreateTransitGatewayPeeringAttachment =
             CreateTransitGatewayPeeringAttachmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTransitGatewayPeeringAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayPeeringAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment
    -- ^ The transit gateway peering attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkCreateTransitGatewayPeeringAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitGatewayPeeringAttachmentResponse
mkCreateTransitGatewayPeeringAttachmentResponse responseStatus
  = CreateTransitGatewayPeeringAttachmentResponse'{transitGatewayPeeringAttachment
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparrsTransitGatewayPeeringAttachment :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
ctgparrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# INLINEABLE ctgparrsTransitGatewayPeeringAttachment #-}
{-# DEPRECATED transitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparrsResponseStatus :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse Core.Int
ctgparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
