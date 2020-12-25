{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AcceptTransitGatewayPeeringAttachment (..),
    mkAcceptTransitGatewayPeeringAttachment,

    -- ** Request lenses
    atgpaTransitGatewayAttachmentId,
    atgpaDryRun,

    -- * Destructuring the response
    AcceptTransitGatewayPeeringAttachmentResponse (..),
    mkAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    atgparrsTransitGatewayPeeringAttachment,
    atgparrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
  { -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptTransitGatewayPeeringAttachment' value with any optional fields omitted.
mkAcceptTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  AcceptTransitGatewayPeeringAttachment
mkAcceptTransitGatewayPeeringAttachment transitGatewayAttachmentId =
  AcceptTransitGatewayPeeringAttachment'
    { transitGatewayAttachmentId,
      dryRun = Core.Nothing
    }

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaTransitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayPeeringAttachment Types.TransitGatewayAttachmentId
atgpaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED atgpaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaDryRun :: Lens.Lens' AcceptTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
atgpaDryRun = Lens.field @"dryRun"
{-# DEPRECATED atgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest AcceptTransitGatewayPeeringAttachment where
  type
    Rs AcceptTransitGatewayPeeringAttachment =
      AcceptTransitGatewayPeeringAttachmentResponse
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
            ( Core.pure ("Action", "AcceptTransitGatewayPeeringAttachment")
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
          AcceptTransitGatewayPeeringAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayPeeringAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Core.Maybe Types.TransitGatewayPeeringAttachment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AcceptTransitGatewayPeeringAttachmentResponse' value with any optional fields omitted.
mkAcceptTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptTransitGatewayPeeringAttachmentResponse
mkAcceptTransitGatewayPeeringAttachmentResponse responseStatus =
  AcceptTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Core.Nothing,
      responseStatus
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparrsTransitGatewayPeeringAttachment :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse (Core.Maybe Types.TransitGatewayPeeringAttachment)
atgparrsTransitGatewayPeeringAttachment = Lens.field @"transitGatewayPeeringAttachment"
{-# DEPRECATED atgparrsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparrsResponseStatus :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse Core.Int
atgparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atgparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
