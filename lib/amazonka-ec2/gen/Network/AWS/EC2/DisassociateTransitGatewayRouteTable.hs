{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
  ( -- * Creating a request
    DisassociateTransitGatewayRouteTable (..),
    mkDisassociateTransitGatewayRouteTable,

    -- ** Request lenses
    dtgrtfTransitGatewayRouteTableId,
    dtgrtfTransitGatewayAttachmentId,
    dtgrtfDryRun,

    -- * Destructuring the response
    DisassociateTransitGatewayRouteTableResponse (..),
    mkDisassociateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    dtgrtrfrsAssociation,
    dtgrtrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayRouteTable' value with any optional fields omitted.
mkDisassociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  DisassociateTransitGatewayRouteTable
mkDisassociateTransitGatewayRouteTable
  transitGatewayRouteTableId
  transitGatewayAttachmentId =
    DisassociateTransitGatewayRouteTable'
      { transitGatewayRouteTableId,
        transitGatewayAttachmentId,
        dryRun = Core.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfTransitGatewayRouteTableId :: Lens.Lens' DisassociateTransitGatewayRouteTable Types.TransitGatewayRouteTableId
dtgrtfTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED dtgrtfTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayRouteTable Types.TransitGatewayAttachmentId
dtgrtfTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED dtgrtfTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfDryRun :: Lens.Lens' DisassociateTransitGatewayRouteTable (Core.Maybe Core.Bool)
dtgrtfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgrtfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DisassociateTransitGatewayRouteTable where
  type
    Rs DisassociateTransitGatewayRouteTable =
      DisassociateTransitGatewayRouteTableResponse
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
            ( Core.pure ("Action", "DisassociateTransitGatewayRouteTable")
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
          DisassociateTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "association") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { -- | Information about the association.
    association :: Core.Maybe Types.TransitGatewayAssociation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkDisassociateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateTransitGatewayRouteTableResponse
mkDisassociateTransitGatewayRouteTableResponse responseStatus =
  DisassociateTransitGatewayRouteTableResponse'
    { association =
        Core.Nothing,
      responseStatus
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrfrsAssociation :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayAssociation)
dtgrtrfrsAssociation = Lens.field @"association"
{-# DEPRECATED dtgrtrfrsAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrfrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse Core.Int
dtgrtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
