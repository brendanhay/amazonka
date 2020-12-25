{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified transit gateway.
module Network.AWS.EC2.CreateTransitGatewayRouteTable
  ( -- * Creating a request
    CreateTransitGatewayRouteTable (..),
    mkCreateTransitGatewayRouteTable,

    -- ** Request lenses
    ctgrtTransitGatewayId,
    ctgrtDryRun,
    ctgrtTagSpecifications,

    -- * Destructuring the response
    CreateTransitGatewayRouteTableResponse (..),
    mkCreateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    ctgrtrrsTransitGatewayRouteTable,
    ctgrtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayRouteTable' smart constructor.
data CreateTransitGatewayRouteTable = CreateTransitGatewayRouteTable'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Types.TransitGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the transit gateway route table.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayRouteTable' value with any optional fields omitted.
mkCreateTransitGatewayRouteTable ::
  -- | 'transitGatewayId'
  Types.TransitGatewayId ->
  CreateTransitGatewayRouteTable
mkCreateTransitGatewayRouteTable transitGatewayId =
  CreateTransitGatewayRouteTable'
    { transitGatewayId,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTransitGatewayId :: Lens.Lens' CreateTransitGatewayRouteTable Types.TransitGatewayId
ctgrtTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED ctgrtTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtDryRun :: Lens.Lens' CreateTransitGatewayRouteTable (Core.Maybe Core.Bool)
ctgrtDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the transit gateway route table.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTagSpecifications :: Lens.Lens' CreateTransitGatewayRouteTable (Core.Maybe [Types.TagSpecification])
ctgrtTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ctgrtTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateTransitGatewayRouteTable where
  type
    Rs CreateTransitGatewayRouteTable =
      CreateTransitGatewayRouteTableResponse
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
            ( Core.pure ("Action", "CreateTransitGatewayRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TransitGatewayId" transitGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecifications" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "transitGatewayRouteTable")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransitGatewayRouteTableResponse' smart constructor.
data CreateTransitGatewayRouteTableResponse = CreateTransitGatewayRouteTableResponse'
  { -- | Information about the transit gateway route table.
    transitGatewayRouteTable :: Core.Maybe Types.TransitGatewayRouteTable,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkCreateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTransitGatewayRouteTableResponse
mkCreateTransitGatewayRouteTableResponse responseStatus =
  CreateTransitGatewayRouteTableResponse'
    { transitGatewayRouteTable =
        Core.Nothing,
      responseStatus
    }

-- | Information about the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrrsTransitGatewayRouteTable :: Lens.Lens' CreateTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayRouteTable)
ctgrtrrsTransitGatewayRouteTable = Lens.field @"transitGatewayRouteTable"
{-# DEPRECATED ctgrtrrsTransitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrrsResponseStatus :: Lens.Lens' CreateTransitGatewayRouteTableResponse Core.Int
ctgrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
