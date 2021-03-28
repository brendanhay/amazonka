{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a transit gateway peering attachment request. The peering attachment must be in the @pendingAcceptance@ state.
module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
    (
    -- * Creating a request
      AcceptTransitGatewayPeeringAttachment (..)
    , mkAcceptTransitGatewayPeeringAttachment
    -- ** Request lenses
    , atgpaTransitGatewayAttachmentId
    , atgpaDryRun

    -- * Destructuring the response
    , AcceptTransitGatewayPeeringAttachmentResponse (..)
    , mkAcceptTransitGatewayPeeringAttachmentResponse
    -- ** Response lenses
    , atgparrsTransitGatewayPeeringAttachment
    , atgparrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the transit gateway attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkAcceptTransitGatewayPeeringAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> AcceptTransitGatewayPeeringAttachment
mkAcceptTransitGatewayPeeringAttachment transitGatewayAttachmentId
  = AcceptTransitGatewayPeeringAttachment'{transitGatewayAttachmentId,
                                           dryRun = Core.Nothing}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaTransitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayPeeringAttachment Types.TransitGatewayAttachmentId
atgpaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE atgpaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaDryRun :: Lens.Lens' AcceptTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
atgpaDryRun = Lens.field @"dryRun"
{-# INLINEABLE atgpaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AcceptTransitGatewayPeeringAttachment where
        toQuery AcceptTransitGatewayPeeringAttachment{..}
          = Core.toQueryPair "Action"
              ("AcceptTransitGatewayPeeringAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AcceptTransitGatewayPeeringAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AcceptTransitGatewayPeeringAttachment
         where
        type Rs AcceptTransitGatewayPeeringAttachment =
             AcceptTransitGatewayPeeringAttachmentResponse
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
                 AcceptTransitGatewayPeeringAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayPeeringAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
  { transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment
    -- ^ The transit gateway peering attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AcceptTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkAcceptTransitGatewayPeeringAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptTransitGatewayPeeringAttachmentResponse
mkAcceptTransitGatewayPeeringAttachmentResponse responseStatus
  = AcceptTransitGatewayPeeringAttachmentResponse'{transitGatewayPeeringAttachment
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparrsTransitGatewayPeeringAttachment :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
atgparrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# INLINEABLE atgparrsTransitGatewayPeeringAttachment #-}
{-# DEPRECATED transitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparrsResponseStatus :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse Core.Int
atgparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atgparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
