{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway.
module Network.AWS.EC2.DeleteTransitGateway
    (
    -- * Creating a request
      DeleteTransitGateway (..)
    , mkDeleteTransitGateway
    -- ** Request lenses
    , dtgTransitGatewayId
    , dtgDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayResponse (..)
    , mkDeleteTransitGatewayResponse
    -- ** Response lenses
    , dtgrrsTransitGateway
    , dtgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGateway' smart constructor.
data DeleteTransitGateway = DeleteTransitGateway'
  { transitGatewayId :: Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGateway' value with any optional fields omitted.
mkDeleteTransitGateway
    :: Types.TransitGatewayId -- ^ 'transitGatewayId'
    -> DeleteTransitGateway
mkDeleteTransitGateway transitGatewayId
  = DeleteTransitGateway'{transitGatewayId, dryRun = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTransitGatewayId :: Lens.Lens' DeleteTransitGateway Types.TransitGatewayId
dtgTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE dtgTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgDryRun :: Lens.Lens' DeleteTransitGateway (Core.Maybe Core.Bool)
dtgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGateway where
        toQuery DeleteTransitGateway{..}
          = Core.toQueryPair "Action" ("DeleteTransitGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "TransitGatewayId" transitGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGateway where
        type Rs DeleteTransitGateway = DeleteTransitGatewayResponse
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
                 DeleteTransitGatewayResponse' Core.<$>
                   (x Core..@? "transitGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayResponse' smart constructor.
data DeleteTransitGatewayResponse = DeleteTransitGatewayResponse'
  { transitGateway :: Core.Maybe Types.TransitGateway
    -- ^ Information about the deleted transit gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteTransitGatewayResponse' value with any optional fields omitted.
mkDeleteTransitGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayResponse
mkDeleteTransitGatewayResponse responseStatus
  = DeleteTransitGatewayResponse'{transitGateway = Core.Nothing,
                                  responseStatus}

-- | Information about the deleted transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsTransitGateway :: Lens.Lens' DeleteTransitGatewayResponse (Core.Maybe Types.TransitGateway)
dtgrrsTransitGateway = Lens.field @"transitGateway"
{-# INLINEABLE dtgrrsTransitGateway #-}
{-# DEPRECATED transitGateway "Use generic-lens or generic-optics with 'transitGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayResponse Core.Int
dtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
