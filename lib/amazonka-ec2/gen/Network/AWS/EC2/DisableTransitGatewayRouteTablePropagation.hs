{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified resource attachment from propagating routes to the specified propagation route table.
module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
    (
    -- * Creating a request
      DisableTransitGatewayRouteTablePropagation (..)
    , mkDisableTransitGatewayRouteTablePropagation
    -- ** Request lenses
    , dtgrtpTransitGatewayRouteTableId
    , dtgrtpTransitGatewayAttachmentId
    , dtgrtpDryRun

    -- * Destructuring the response
    , DisableTransitGatewayRouteTablePropagationResponse (..)
    , mkDisableTransitGatewayRouteTablePropagationResponse
    -- ** Response lenses
    , dtgrtprrsPropagation
    , dtgrtprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagation' smart constructor.
data DisableTransitGatewayRouteTablePropagation = DisableTransitGatewayRouteTablePropagation'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the propagation route table.
  , transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTransitGatewayRouteTablePropagation' value with any optional fields omitted.
mkDisableTransitGatewayRouteTablePropagation
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> DisableTransitGatewayRouteTablePropagation
mkDisableTransitGatewayRouteTablePropagation
  transitGatewayRouteTableId transitGatewayAttachmentId
  = DisableTransitGatewayRouteTablePropagation'{transitGatewayRouteTableId,
                                                transitGatewayAttachmentId, dryRun = Core.Nothing}

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayRouteTableId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Types.TransitGatewayRouteTableId
dtgrtpTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE dtgrtpTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayAttachmentId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Types.TransitGatewayAttachmentId
dtgrtpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE dtgrtpTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpDryRun :: Lens.Lens' DisableTransitGatewayRouteTablePropagation (Core.Maybe Core.Bool)
dtgrtpDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgrtpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisableTransitGatewayRouteTablePropagation
         where
        toQuery DisableTransitGatewayRouteTablePropagation{..}
          = Core.toQueryPair "Action"
              ("DisableTransitGatewayRouteTablePropagation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisableTransitGatewayRouteTablePropagation
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableTransitGatewayRouteTablePropagation
         where
        type Rs DisableTransitGatewayRouteTablePropagation =
             DisableTransitGatewayRouteTablePropagationResponse
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
                 DisableTransitGatewayRouteTablePropagationResponse' Core.<$>
                   (x Core..@? "propagation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagationResponse' smart constructor.
data DisableTransitGatewayRouteTablePropagationResponse = DisableTransitGatewayRouteTablePropagationResponse'
  { propagation :: Core.Maybe Types.TransitGatewayPropagation
    -- ^ Information about route propagation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTransitGatewayRouteTablePropagationResponse' value with any optional fields omitted.
mkDisableTransitGatewayRouteTablePropagationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableTransitGatewayRouteTablePropagationResponse
mkDisableTransitGatewayRouteTablePropagationResponse responseStatus
  = DisableTransitGatewayRouteTablePropagationResponse'{propagation =
                                                          Core.Nothing,
                                                        responseStatus}

-- | Information about route propagation.
--
-- /Note:/ Consider using 'propagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprrsPropagation :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse (Core.Maybe Types.TransitGatewayPropagation)
dtgrtprrsPropagation = Lens.field @"propagation"
{-# INLINEABLE dtgrtprrsPropagation #-}
{-# DEPRECATED propagation "Use generic-lens or generic-optics with 'propagation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprrsResponseStatus :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse Core.Int
dtgrtprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrtprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
