{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use 'DescribeTransitGatewayVpcAttachments' to view your pending VPC attachment requests. Use 'RejectTransitGatewayVpcAttachment' to reject a VPC attachment request.
module Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
    (
    -- * Creating a request
      AcceptTransitGatewayVpcAttachment (..)
    , mkAcceptTransitGatewayVpcAttachment
    -- ** Request lenses
    , atgvaTransitGatewayAttachmentId
    , atgvaDryRun

    -- * Destructuring the response
    , AcceptTransitGatewayVpcAttachmentResponse (..)
    , mkAcceptTransitGatewayVpcAttachmentResponse
    -- ** Response lenses
    , atgvarrsTransitGatewayVpcAttachment
    , atgvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptTransitGatewayVpcAttachment' smart constructor.
data AcceptTransitGatewayVpcAttachment = AcceptTransitGatewayVpcAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptTransitGatewayVpcAttachment' value with any optional fields omitted.
mkAcceptTransitGatewayVpcAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> AcceptTransitGatewayVpcAttachment
mkAcceptTransitGatewayVpcAttachment transitGatewayAttachmentId
  = AcceptTransitGatewayVpcAttachment'{transitGatewayAttachmentId,
                                       dryRun = Core.Nothing}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvaTransitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayVpcAttachment Types.TransitGatewayAttachmentId
atgvaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE atgvaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvaDryRun :: Lens.Lens' AcceptTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
atgvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE atgvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AcceptTransitGatewayVpcAttachment where
        toQuery AcceptTransitGatewayVpcAttachment{..}
          = Core.toQueryPair "Action"
              ("AcceptTransitGatewayVpcAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AcceptTransitGatewayVpcAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AcceptTransitGatewayVpcAttachment where
        type Rs AcceptTransitGatewayVpcAttachment =
             AcceptTransitGatewayVpcAttachmentResponse
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
                 AcceptTransitGatewayVpcAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayVpcAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptTransitGatewayVpcAttachmentResponse' smart constructor.
data AcceptTransitGatewayVpcAttachmentResponse = AcceptTransitGatewayVpcAttachmentResponse'
  { transitGatewayVpcAttachment :: Core.Maybe Types.TransitGatewayVpcAttachment
    -- ^ The VPC attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AcceptTransitGatewayVpcAttachmentResponse' value with any optional fields omitted.
mkAcceptTransitGatewayVpcAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptTransitGatewayVpcAttachmentResponse
mkAcceptTransitGatewayVpcAttachmentResponse responseStatus
  = AcceptTransitGatewayVpcAttachmentResponse'{transitGatewayVpcAttachment
                                                 = Core.Nothing,
                                               responseStatus}

-- | The VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvarrsTransitGatewayVpcAttachment :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse (Core.Maybe Types.TransitGatewayVpcAttachment)
atgvarrsTransitGatewayVpcAttachment = Lens.field @"transitGatewayVpcAttachment"
{-# INLINEABLE atgvarrsTransitGatewayVpcAttachment #-}
{-# DEPRECATED transitGatewayVpcAttachment "Use generic-lens or generic-optics with 'transitGatewayVpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvarrsResponseStatus :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse Core.Int
atgvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atgvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
