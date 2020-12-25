{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisableTransitGatewayRouteTablePropagation (..),
    mkDisableTransitGatewayRouteTablePropagation,

    -- ** Request lenses
    dtgrtpTransitGatewayRouteTableId,
    dtgrtpTransitGatewayAttachmentId,
    dtgrtpDryRun,

    -- * Destructuring the response
    DisableTransitGatewayRouteTablePropagationResponse (..),
    mkDisableTransitGatewayRouteTablePropagationResponse,

    -- ** Response lenses
    dtgrtprrsPropagation,
    dtgrtprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagation' smart constructor.
data DisableTransitGatewayRouteTablePropagation = DisableTransitGatewayRouteTablePropagation'
  { -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTransitGatewayRouteTablePropagation' value with any optional fields omitted.
mkDisableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  DisableTransitGatewayRouteTablePropagation
mkDisableTransitGatewayRouteTablePropagation
  transitGatewayRouteTableId
  transitGatewayAttachmentId =
    DisableTransitGatewayRouteTablePropagation'
      { transitGatewayRouteTableId,
        transitGatewayAttachmentId,
        dryRun = Core.Nothing
      }

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayRouteTableId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Types.TransitGatewayRouteTableId
dtgrtpTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED dtgrtpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpTransitGatewayAttachmentId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Types.TransitGatewayAttachmentId
dtgrtpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED dtgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtpDryRun :: Lens.Lens' DisableTransitGatewayRouteTablePropagation (Core.Maybe Core.Bool)
dtgrtpDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgrtpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DisableTransitGatewayRouteTablePropagation where
  type
    Rs DisableTransitGatewayRouteTablePropagation =
      DisableTransitGatewayRouteTablePropagationResponse
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
            ( Core.pure ("Action", "DisableTransitGatewayRouteTablePropagation")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> ( Core.toQueryValue
                            "TransitGatewayAttachmentId"
                            transitGatewayAttachmentId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DisableTransitGatewayRouteTablePropagationResponse'
            Core.<$> (x Core..@? "propagation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableTransitGatewayRouteTablePropagationResponse' smart constructor.
data DisableTransitGatewayRouteTablePropagationResponse = DisableTransitGatewayRouteTablePropagationResponse'
  { -- | Information about route propagation.
    propagation :: Core.Maybe Types.TransitGatewayPropagation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTransitGatewayRouteTablePropagationResponse' value with any optional fields omitted.
mkDisableTransitGatewayRouteTablePropagationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableTransitGatewayRouteTablePropagationResponse
mkDisableTransitGatewayRouteTablePropagationResponse responseStatus =
  DisableTransitGatewayRouteTablePropagationResponse'
    { propagation =
        Core.Nothing,
      responseStatus
    }

-- | Information about route propagation.
--
-- /Note:/ Consider using 'propagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprrsPropagation :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse (Core.Maybe Types.TransitGatewayPropagation)
dtgrtprrsPropagation = Lens.field @"propagation"
{-# DEPRECATED dtgrtprrsPropagation "Use generic-lens or generic-optics with 'propagation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtprrsResponseStatus :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse Core.Int
dtgrtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
