{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
module Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
    (
    -- * Creating a request
      DeleteTransitGatewayVpcAttachment (..)
    , mkDeleteTransitGatewayVpcAttachment
    -- ** Request lenses
    , dtgvaTransitGatewayAttachmentId
    , dtgvaDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayVpcAttachmentResponse (..)
    , mkDeleteTransitGatewayVpcAttachmentResponse
    -- ** Response lenses
    , dtgvarrsTransitGatewayVpcAttachment
    , dtgvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayVpcAttachment' smart constructor.
data DeleteTransitGatewayVpcAttachment = DeleteTransitGatewayVpcAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayVpcAttachment' value with any optional fields omitted.
mkDeleteTransitGatewayVpcAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> DeleteTransitGatewayVpcAttachment
mkDeleteTransitGatewayVpcAttachment transitGatewayAttachmentId
  = DeleteTransitGatewayVpcAttachment'{transitGatewayAttachmentId,
                                       dryRun = Core.Nothing}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayVpcAttachment Types.TransitGatewayAttachmentId
dtgvaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE dtgvaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaDryRun :: Lens.Lens' DeleteTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
dtgvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGatewayVpcAttachment where
        toQuery DeleteTransitGatewayVpcAttachment{..}
          = Core.toQueryPair "Action"
              ("DeleteTransitGatewayVpcAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGatewayVpcAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGatewayVpcAttachment where
        type Rs DeleteTransitGatewayVpcAttachment =
             DeleteTransitGatewayVpcAttachmentResponse
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
                 DeleteTransitGatewayVpcAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayVpcAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayVpcAttachmentResponse' smart constructor.
data DeleteTransitGatewayVpcAttachmentResponse = DeleteTransitGatewayVpcAttachmentResponse'
  { transitGatewayVpcAttachment :: Core.Maybe Types.TransitGatewayVpcAttachment
    -- ^ Information about the deleted VPC attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteTransitGatewayVpcAttachmentResponse' value with any optional fields omitted.
mkDeleteTransitGatewayVpcAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayVpcAttachmentResponse
mkDeleteTransitGatewayVpcAttachmentResponse responseStatus
  = DeleteTransitGatewayVpcAttachmentResponse'{transitGatewayVpcAttachment
                                                 = Core.Nothing,
                                               responseStatus}

-- | Information about the deleted VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarrsTransitGatewayVpcAttachment :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse (Core.Maybe Types.TransitGatewayVpcAttachment)
dtgvarrsTransitGatewayVpcAttachment = Lens.field @"transitGatewayVpcAttachment"
{-# INLINEABLE dtgvarrsTransitGatewayVpcAttachment #-}
{-# DEPRECATED transitGatewayVpcAttachment "Use generic-lens or generic-optics with 'transitGatewayVpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarrsResponseStatus :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse Core.Int
dtgvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
