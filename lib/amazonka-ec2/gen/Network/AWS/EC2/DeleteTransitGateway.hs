{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteTransitGateway (..),
    mkDeleteTransitGateway,

    -- ** Request lenses
    dtgTransitGatewayId,
    dtgDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayResponse (..),
    mkDeleteTransitGatewayResponse,

    -- ** Response lenses
    dtgrrsTransitGateway,
    dtgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGateway' smart constructor.
data DeleteTransitGateway = DeleteTransitGateway'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Types.TransitGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGateway' value with any optional fields omitted.
mkDeleteTransitGateway ::
  -- | 'transitGatewayId'
  Types.TransitGatewayId ->
  DeleteTransitGateway
mkDeleteTransitGateway transitGatewayId =
  DeleteTransitGateway' {transitGatewayId, dryRun = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTransitGatewayId :: Lens.Lens' DeleteTransitGateway Types.TransitGatewayId
dtgTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED dtgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgDryRun :: Lens.Lens' DeleteTransitGateway (Core.Maybe Core.Bool)
dtgDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGateway where
  type Rs DeleteTransitGateway = DeleteTransitGatewayResponse
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
            ( Core.pure ("Action", "DeleteTransitGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TransitGatewayId" transitGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayResponse'
            Core.<$> (x Core..@? "transitGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayResponse' smart constructor.
data DeleteTransitGatewayResponse = DeleteTransitGatewayResponse'
  { -- | Information about the deleted transit gateway.
    transitGateway :: Core.Maybe Types.TransitGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteTransitGatewayResponse' value with any optional fields omitted.
mkDeleteTransitGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayResponse
mkDeleteTransitGatewayResponse responseStatus =
  DeleteTransitGatewayResponse'
    { transitGateway = Core.Nothing,
      responseStatus
    }

-- | Information about the deleted transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsTransitGateway :: Lens.Lens' DeleteTransitGatewayResponse (Core.Maybe Types.TransitGateway)
dtgrrsTransitGateway = Lens.field @"transitGateway"
{-# DEPRECATED dtgrrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayResponse Core.Int
dtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
