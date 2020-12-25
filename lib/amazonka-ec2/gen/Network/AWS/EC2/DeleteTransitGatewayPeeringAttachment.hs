{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
  ( -- * Creating a request
    DeleteTransitGatewayPeeringAttachment (..),
    mkDeleteTransitGatewayPeeringAttachment,

    -- ** Request lenses
    dtgpafTransitGatewayAttachmentId,
    dtgpafDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayPeeringAttachmentResponse (..),
    mkDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    dtgparfrsTransitGatewayPeeringAttachment,
    dtgparfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'
  { -- | The ID of the transit gateway peering attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkDeleteTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  DeleteTransitGatewayPeeringAttachment
mkDeleteTransitGatewayPeeringAttachment transitGatewayAttachmentId =
  DeleteTransitGatewayPeeringAttachment'
    { transitGatewayAttachmentId,
      dryRun = Core.Nothing
    }

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpafTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayPeeringAttachment Types.TransitGatewayAttachmentId
dtgpafTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED dtgpafTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpafDryRun :: Lens.Lens' DeleteTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
dtgpafDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgpafDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGatewayPeeringAttachment where
  type
    Rs DeleteTransitGatewayPeeringAttachment =
      DeleteTransitGatewayPeeringAttachmentResponse
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
            ( Core.pure ("Action", "DeleteTransitGatewayPeeringAttachment")
                Core.<> (Core.pure ("Version", "2016-11-15"))
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
          DeleteTransitGatewayPeeringAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayPeeringAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkDeleteTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayPeeringAttachmentResponse
mkDeleteTransitGatewayPeeringAttachmentResponse responseStatus =
  DeleteTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Core.Nothing,
      responseStatus
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparfrsTransitGatewayPeeringAttachment :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
dtgparfrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# DEPRECATED dtgparfrsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparfrsResponseStatus :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse Core.Int
dtgparfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgparfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
