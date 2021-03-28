{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
    (
    -- * Creating a request
      DeleteTransitGatewayPeeringAttachment (..)
    , mkDeleteTransitGatewayPeeringAttachment
    -- ** Request lenses
    , dtgpafTransitGatewayAttachmentId
    , dtgpafDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayPeeringAttachmentResponse (..)
    , mkDeleteTransitGatewayPeeringAttachmentResponse
    -- ** Response lenses
    , dtgparfrsTransitGatewayPeeringAttachment
    , dtgparfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the transit gateway peering attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkDeleteTransitGatewayPeeringAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> DeleteTransitGatewayPeeringAttachment
mkDeleteTransitGatewayPeeringAttachment transitGatewayAttachmentId
  = DeleteTransitGatewayPeeringAttachment'{transitGatewayAttachmentId,
                                           dryRun = Core.Nothing}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpafTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayPeeringAttachment Types.TransitGatewayAttachmentId
dtgpafTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE dtgpafTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpafDryRun :: Lens.Lens' DeleteTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
dtgpafDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgpafDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGatewayPeeringAttachment where
        toQuery DeleteTransitGatewayPeeringAttachment{..}
          = Core.toQueryPair "Action"
              ("DeleteTransitGatewayPeeringAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGatewayPeeringAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGatewayPeeringAttachment
         where
        type Rs DeleteTransitGatewayPeeringAttachment =
             DeleteTransitGatewayPeeringAttachmentResponse
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
                 DeleteTransitGatewayPeeringAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayPeeringAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'
  { transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment
    -- ^ The transit gateway peering attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkDeleteTransitGatewayPeeringAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayPeeringAttachmentResponse
mkDeleteTransitGatewayPeeringAttachmentResponse responseStatus
  = DeleteTransitGatewayPeeringAttachmentResponse'{transitGatewayPeeringAttachment
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparfrsTransitGatewayPeeringAttachment :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
dtgparfrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# INLINEABLE dtgparfrsTransitGatewayPeeringAttachment #-}
{-# DEPRECATED transitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparfrsResponseStatus :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse Core.Int
dtgparfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgparfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
