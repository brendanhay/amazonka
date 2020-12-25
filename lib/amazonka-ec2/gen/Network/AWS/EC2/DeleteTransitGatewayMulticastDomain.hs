{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
  ( -- * Creating a request
    DeleteTransitGatewayMulticastDomain (..),
    mkDeleteTransitGatewayMulticastDomain,

    -- ** Request lenses
    dtgmdTransitGatewayMulticastDomainId,
    dtgmdDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayMulticastDomainResponse (..),
    mkDeleteTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    dtgmdrrsTransitGatewayMulticastDomain,
    dtgmdrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Types.TransitGatewayMulticastDomainId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayMulticastDomain' value with any optional fields omitted.
mkDeleteTransitGatewayMulticastDomain ::
  -- | 'transitGatewayMulticastDomainId'
  Types.TransitGatewayMulticastDomainId ->
  DeleteTransitGatewayMulticastDomain
mkDeleteTransitGatewayMulticastDomain
  transitGatewayMulticastDomainId =
    DeleteTransitGatewayMulticastDomain'
      { transitGatewayMulticastDomainId,
        dryRun = Core.Nothing
      }

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdTransitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Types.TransitGatewayMulticastDomainId
dtgmdTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED dtgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdDryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
dtgmdDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGatewayMulticastDomain where
  type
    Rs DeleteTransitGatewayMulticastDomain =
      DeleteTransitGatewayMulticastDomainResponse
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
            ( Core.pure ("Action", "DeleteTransitGatewayMulticastDomain")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayMulticastDomainId"
                            transitGatewayMulticastDomainId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "transitGatewayMulticastDomain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { -- | Information about the deleted transit gateway multicast domain.
    transitGatewayMulticastDomain :: Core.Maybe Types.TransitGatewayMulticastDomain,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkDeleteTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayMulticastDomainResponse
mkDeleteTransitGatewayMulticastDomainResponse responseStatus =
  DeleteTransitGatewayMulticastDomainResponse'
    { transitGatewayMulticastDomain =
        Core.Nothing,
      responseStatus
    }

-- | Information about the deleted transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrrsTransitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomain)
dtgmdrrsTransitGatewayMulticastDomain = Lens.field @"transitGatewayMulticastDomain"
{-# DEPRECATED dtgmdrrsTransitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Core.Int
dtgmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
