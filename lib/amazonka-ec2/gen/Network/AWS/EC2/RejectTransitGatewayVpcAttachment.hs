{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use 'DescribeTransitGatewayVpcAttachments' to view your pending VPC attachment requests. Use 'AcceptTransitGatewayVpcAttachment' to accept a VPC attachment request.
module Network.AWS.EC2.RejectTransitGatewayVpcAttachment
    (
    -- * Creating a request
      RejectTransitGatewayVpcAttachment (..)
    , mkRejectTransitGatewayVpcAttachment
    -- ** Request lenses
    , rtgvaTransitGatewayAttachmentId
    , rtgvaDryRun

    -- * Destructuring the response
    , RejectTransitGatewayVpcAttachmentResponse (..)
    , mkRejectTransitGatewayVpcAttachmentResponse
    -- ** Response lenses
    , rtgvarrsTransitGatewayVpcAttachment
    , rtgvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectTransitGatewayVpcAttachment' smart constructor.
data RejectTransitGatewayVpcAttachment = RejectTransitGatewayVpcAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectTransitGatewayVpcAttachment' value with any optional fields omitted.
mkRejectTransitGatewayVpcAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> RejectTransitGatewayVpcAttachment
mkRejectTransitGatewayVpcAttachment transitGatewayAttachmentId
  = RejectTransitGatewayVpcAttachment'{transitGatewayAttachmentId,
                                       dryRun = Core.Nothing}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvaTransitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayVpcAttachment Types.TransitGatewayAttachmentId
rtgvaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE rtgvaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvaDryRun :: Lens.Lens' RejectTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
rtgvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rtgvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RejectTransitGatewayVpcAttachment where
        toQuery RejectTransitGatewayVpcAttachment{..}
          = Core.toQueryPair "Action"
              ("RejectTransitGatewayVpcAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RejectTransitGatewayVpcAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RejectTransitGatewayVpcAttachment where
        type Rs RejectTransitGatewayVpcAttachment =
             RejectTransitGatewayVpcAttachmentResponse
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
                 RejectTransitGatewayVpcAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayVpcAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectTransitGatewayVpcAttachmentResponse' smart constructor.
data RejectTransitGatewayVpcAttachmentResponse = RejectTransitGatewayVpcAttachmentResponse'
  { transitGatewayVpcAttachment :: Core.Maybe Types.TransitGatewayVpcAttachment
    -- ^ Information about the attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RejectTransitGatewayVpcAttachmentResponse' value with any optional fields omitted.
mkRejectTransitGatewayVpcAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectTransitGatewayVpcAttachmentResponse
mkRejectTransitGatewayVpcAttachmentResponse responseStatus
  = RejectTransitGatewayVpcAttachmentResponse'{transitGatewayVpcAttachment
                                                 = Core.Nothing,
                                               responseStatus}

-- | Information about the attachment.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvarrsTransitGatewayVpcAttachment :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse (Core.Maybe Types.TransitGatewayVpcAttachment)
rtgvarrsTransitGatewayVpcAttachment = Lens.field @"transitGatewayVpcAttachment"
{-# INLINEABLE rtgvarrsTransitGatewayVpcAttachment #-}
{-# DEPRECATED transitGatewayVpcAttachment "Use generic-lens or generic-optics with 'transitGatewayVpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvarrsResponseStatus :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse Core.Int
rtgvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtgvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
