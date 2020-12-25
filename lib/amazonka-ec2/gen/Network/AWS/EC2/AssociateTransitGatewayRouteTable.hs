{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified attachment with the specified transit gateway route table. You can associate only one route table with an attachment.
module Network.AWS.EC2.AssociateTransitGatewayRouteTable
  ( -- * Creating a request
    AssociateTransitGatewayRouteTable (..),
    mkAssociateTransitGatewayRouteTable,

    -- ** Request lenses
    atgrtTransitGatewayRouteTableId,
    atgrtTransitGatewayAttachmentId,
    atgrtDryRun,

    -- * Destructuring the response
    AssociateTransitGatewayRouteTableResponse (..),
    mkAssociateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    atgrtrrsAssociation,
    atgrtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateTransitGatewayRouteTable' smart constructor.
data AssociateTransitGatewayRouteTable = AssociateTransitGatewayRouteTable'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTransitGatewayRouteTable' value with any optional fields omitted.
mkAssociateTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  AssociateTransitGatewayRouteTable
mkAssociateTransitGatewayRouteTable
  transitGatewayRouteTableId
  transitGatewayAttachmentId =
    AssociateTransitGatewayRouteTable'
      { transitGatewayRouteTableId,
        transitGatewayAttachmentId,
        dryRun = Core.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtTransitGatewayRouteTableId :: Lens.Lens' AssociateTransitGatewayRouteTable Types.TransitGatewayRouteTableId
atgrtTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED atgrtTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtTransitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayRouteTable Types.TransitGatewayAttachmentId
atgrtTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED atgrtTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtDryRun :: Lens.Lens' AssociateTransitGatewayRouteTable (Core.Maybe Core.Bool)
atgrtDryRun = Lens.field @"dryRun"
{-# DEPRECATED atgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest AssociateTransitGatewayRouteTable where
  type
    Rs AssociateTransitGatewayRouteTable =
      AssociateTransitGatewayRouteTableResponse
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
            ( Core.pure ("Action", "AssociateTransitGatewayRouteTable")
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
          AssociateTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "association") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTransitGatewayRouteTableResponse' smart constructor.
data AssociateTransitGatewayRouteTableResponse = AssociateTransitGatewayRouteTableResponse'
  { -- | The ID of the association.
    association :: Core.Maybe Types.TransitGatewayAssociation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkAssociateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTransitGatewayRouteTableResponse
mkAssociateTransitGatewayRouteTableResponse responseStatus =
  AssociateTransitGatewayRouteTableResponse'
    { association =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtrrsAssociation :: Lens.Lens' AssociateTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayAssociation)
atgrtrrsAssociation = Lens.field @"association"
{-# DEPRECATED atgrtrrsAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgrtrrsResponseStatus :: Lens.Lens' AssociateTransitGatewayRouteTableResponse Core.Int
atgrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atgrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
