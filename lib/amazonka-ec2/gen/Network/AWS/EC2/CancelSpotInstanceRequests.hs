{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot Instance requests.
--
-- /Important:/ Canceling a Spot Instance request does not terminate running Spot Instances associated with the request.
module Network.AWS.EC2.CancelSpotInstanceRequests
  ( -- * Creating a request
    CancelSpotInstanceRequests (..),
    mkCancelSpotInstanceRequests,

    -- ** Request lenses
    csirSpotInstanceRequestIds,
    csirDryRun,

    -- * Destructuring the response
    CancelSpotInstanceRequestsResponse (..),
    mkCancelSpotInstanceRequestsResponse,

    -- ** Response lenses
    csirrrsCancelledSpotInstanceRequests,
    csirrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'mkCancelSpotInstanceRequests' smart constructor.
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
  { -- | One or more Spot Instance request IDs.
    spotInstanceRequestIds :: [Types.SpotInstanceRequestId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotInstanceRequests' value with any optional fields omitted.
mkCancelSpotInstanceRequests ::
  CancelSpotInstanceRequests
mkCancelSpotInstanceRequests =
  CancelSpotInstanceRequests'
    { spotInstanceRequestIds = Core.mempty,
      dryRun = Core.Nothing
    }

-- | One or more Spot Instance request IDs.
--
-- /Note:/ Consider using 'spotInstanceRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirSpotInstanceRequestIds :: Lens.Lens' CancelSpotInstanceRequests [Types.SpotInstanceRequestId]
csirSpotInstanceRequestIds = Lens.field @"spotInstanceRequestIds"
{-# DEPRECATED csirSpotInstanceRequestIds "Use generic-lens or generic-optics with 'spotInstanceRequestIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirDryRun :: Lens.Lens' CancelSpotInstanceRequests (Core.Maybe Core.Bool)
csirDryRun = Lens.field @"dryRun"
{-# DEPRECATED csirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest CancelSpotInstanceRequests where
  type
    Rs CancelSpotInstanceRequests =
      CancelSpotInstanceRequestsResponse
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
            ( Core.pure ("Action", "CancelSpotInstanceRequests")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "SpotInstanceRequestId" spotInstanceRequestIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotInstanceRequestsResponse'
            Core.<$> ( x Core..@? "spotInstanceRequestSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'mkCancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
  { -- | One or more Spot Instance requests.
    cancelledSpotInstanceRequests :: Core.Maybe [Types.CancelledSpotInstanceRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotInstanceRequestsResponse' value with any optional fields omitted.
mkCancelSpotInstanceRequestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelSpotInstanceRequestsResponse
mkCancelSpotInstanceRequestsResponse responseStatus =
  CancelSpotInstanceRequestsResponse'
    { cancelledSpotInstanceRequests =
        Core.Nothing,
      responseStatus
    }

-- | One or more Spot Instance requests.
--
-- /Note:/ Consider using 'cancelledSpotInstanceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrrsCancelledSpotInstanceRequests :: Lens.Lens' CancelSpotInstanceRequestsResponse (Core.Maybe [Types.CancelledSpotInstanceRequest])
csirrrsCancelledSpotInstanceRequests = Lens.field @"cancelledSpotInstanceRequests"
{-# DEPRECATED csirrrsCancelledSpotInstanceRequests "Use generic-lens or generic-optics with 'cancelledSpotInstanceRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrrsResponseStatus :: Lens.Lens' CancelSpotInstanceRequestsResponse Core.Int
csirrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
