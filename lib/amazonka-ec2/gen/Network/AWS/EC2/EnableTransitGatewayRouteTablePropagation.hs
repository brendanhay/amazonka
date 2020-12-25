{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified attachment to propagate routes to the specified propagation route table.
module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
  ( -- * Creating a request
    EnableTransitGatewayRouteTablePropagation (..),
    mkEnableTransitGatewayRouteTablePropagation,

    -- ** Request lenses
    etgrtpTransitGatewayRouteTableId,
    etgrtpTransitGatewayAttachmentId,
    etgrtpDryRun,

    -- * Destructuring the response
    EnableTransitGatewayRouteTablePropagationResponse (..),
    mkEnableTransitGatewayRouteTablePropagationResponse,

    -- ** Response lenses
    etgrtprrsPropagation,
    etgrtprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableTransitGatewayRouteTablePropagation' smart constructor.
data EnableTransitGatewayRouteTablePropagation = EnableTransitGatewayRouteTablePropagation'
  { -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableTransitGatewayRouteTablePropagation' value with any optional fields omitted.
mkEnableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  EnableTransitGatewayRouteTablePropagation
mkEnableTransitGatewayRouteTablePropagation
  transitGatewayRouteTableId
  transitGatewayAttachmentId =
    EnableTransitGatewayRouteTablePropagation'
      { transitGatewayRouteTableId,
        transitGatewayAttachmentId,
        dryRun = Core.Nothing
      }

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpTransitGatewayRouteTableId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Types.TransitGatewayRouteTableId
etgrtpTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED etgrtpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpTransitGatewayAttachmentId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Types.TransitGatewayAttachmentId
etgrtpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED etgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtpDryRun :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Core.Maybe Core.Bool)
etgrtpDryRun = Lens.field @"dryRun"
{-# DEPRECATED etgrtpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest EnableTransitGatewayRouteTablePropagation where
  type
    Rs EnableTransitGatewayRouteTablePropagation =
      EnableTransitGatewayRouteTablePropagationResponse
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
            ( Core.pure ("Action", "EnableTransitGatewayRouteTablePropagation")
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
          EnableTransitGatewayRouteTablePropagationResponse'
            Core.<$> (x Core..@? "propagation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableTransitGatewayRouteTablePropagationResponse' smart constructor.
data EnableTransitGatewayRouteTablePropagationResponse = EnableTransitGatewayRouteTablePropagationResponse'
  { -- | Information about route propagation.
    propagation :: Core.Maybe Types.TransitGatewayPropagation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableTransitGatewayRouteTablePropagationResponse' value with any optional fields omitted.
mkEnableTransitGatewayRouteTablePropagationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableTransitGatewayRouteTablePropagationResponse
mkEnableTransitGatewayRouteTablePropagationResponse responseStatus =
  EnableTransitGatewayRouteTablePropagationResponse'
    { propagation =
        Core.Nothing,
      responseStatus
    }

-- | Information about route propagation.
--
-- /Note:/ Consider using 'propagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtprrsPropagation :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse (Core.Maybe Types.TransitGatewayPropagation)
etgrtprrsPropagation = Lens.field @"propagation"
{-# DEPRECATED etgrtprrsPropagation "Use generic-lens or generic-optics with 'propagation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etgrtprrsResponseStatus :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse Core.Int
etgrtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED etgrtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
