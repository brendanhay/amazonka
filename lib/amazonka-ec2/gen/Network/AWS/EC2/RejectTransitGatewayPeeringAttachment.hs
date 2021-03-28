{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a transit gateway peering attachment request.
module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
    (
    -- * Creating a request
      RejectTransitGatewayPeeringAttachment (..)
    , mkRejectTransitGatewayPeeringAttachment
    -- ** Request lenses
    , rtgpaTransitGatewayAttachmentId
    , rtgpaDryRun

    -- * Destructuring the response
    , RejectTransitGatewayPeeringAttachmentResponse (..)
    , mkRejectTransitGatewayPeeringAttachmentResponse
    -- ** Response lenses
    , rtgparrsTransitGatewayPeeringAttachment
    , rtgparrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectTransitGatewayPeeringAttachment' smart constructor.
data RejectTransitGatewayPeeringAttachment = RejectTransitGatewayPeeringAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the transit gateway peering attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkRejectTransitGatewayPeeringAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> RejectTransitGatewayPeeringAttachment
mkRejectTransitGatewayPeeringAttachment transitGatewayAttachmentId
  = RejectTransitGatewayPeeringAttachment'{transitGatewayAttachmentId,
                                           dryRun = Core.Nothing}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgpaTransitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayPeeringAttachment Types.TransitGatewayAttachmentId
rtgpaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE rtgpaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgpaDryRun :: Lens.Lens' RejectTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
rtgpaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rtgpaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RejectTransitGatewayPeeringAttachment where
        toQuery RejectTransitGatewayPeeringAttachment{..}
          = Core.toQueryPair "Action"
              ("RejectTransitGatewayPeeringAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RejectTransitGatewayPeeringAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RejectTransitGatewayPeeringAttachment
         where
        type Rs RejectTransitGatewayPeeringAttachment =
             RejectTransitGatewayPeeringAttachmentResponse
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
                 RejectTransitGatewayPeeringAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayPeeringAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectTransitGatewayPeeringAttachmentResponse' smart constructor.
data RejectTransitGatewayPeeringAttachmentResponse = RejectTransitGatewayPeeringAttachmentResponse'
  { transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment
    -- ^ The transit gateway peering attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RejectTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkRejectTransitGatewayPeeringAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectTransitGatewayPeeringAttachmentResponse
mkRejectTransitGatewayPeeringAttachmentResponse responseStatus
  = RejectTransitGatewayPeeringAttachmentResponse'{transitGatewayPeeringAttachment
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgparrsTransitGatewayPeeringAttachment :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
rtgparrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# INLINEABLE rtgparrsTransitGatewayPeeringAttachment #-}
{-# DEPRECATED transitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgparrsResponseStatus :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse Core.Int
rtgparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtgparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
