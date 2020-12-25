{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Spot Fleet requests.
--
-- After you cancel a Spot Fleet request, the Spot Fleet launches no new Spot Instances. You must specify whether the Spot Fleet should also terminate its Spot Instances. If you terminate the instances, the Spot Fleet request enters the @cancelled_terminating@ state. Otherwise, the Spot Fleet request enters the @cancelled_running@ state and the instances continue to run until they are interrupted or you terminate them manually.
module Network.AWS.EC2.CancelSpotFleetRequests
  ( -- * Creating a request
    CancelSpotFleetRequests (..),
    mkCancelSpotFleetRequests,

    -- ** Request lenses
    csfrSpotFleetRequestIds,
    csfrTerminateInstances,
    csfrDryRun,

    -- * Destructuring the response
    CancelSpotFleetRequestsResponse (..),
    mkCancelSpotFleetRequestsResponse,

    -- ** Response lenses
    csfrrrsSuccessfulFleetRequests,
    csfrrrsUnsuccessfulFleetRequests,
    csfrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelSpotFleetRequests.
--
-- /See:/ 'mkCancelSpotFleetRequests' smart constructor.
data CancelSpotFleetRequests = CancelSpotFleetRequests'
  { -- | The IDs of the Spot Fleet requests.
    spotFleetRequestIds :: [Types.SpotFleetRequestId],
    -- | Indicates whether to terminate instances for a Spot Fleet request if it is canceled successfully.
    terminateInstances :: Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequests' value with any optional fields omitted.
mkCancelSpotFleetRequests ::
  -- | 'terminateInstances'
  Core.Bool ->
  CancelSpotFleetRequests
mkCancelSpotFleetRequests terminateInstances =
  CancelSpotFleetRequests'
    { spotFleetRequestIds = Core.mempty,
      terminateInstances,
      dryRun = Core.Nothing
    }

-- | The IDs of the Spot Fleet requests.
--
-- /Note:/ Consider using 'spotFleetRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrSpotFleetRequestIds :: Lens.Lens' CancelSpotFleetRequests [Types.SpotFleetRequestId]
csfrSpotFleetRequestIds = Lens.field @"spotFleetRequestIds"
{-# DEPRECATED csfrSpotFleetRequestIds "Use generic-lens or generic-optics with 'spotFleetRequestIds' instead." #-}

-- | Indicates whether to terminate instances for a Spot Fleet request if it is canceled successfully.
--
-- /Note:/ Consider using 'terminateInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrTerminateInstances :: Lens.Lens' CancelSpotFleetRequests Core.Bool
csfrTerminateInstances = Lens.field @"terminateInstances"
{-# DEPRECATED csfrTerminateInstances "Use generic-lens or generic-optics with 'terminateInstances' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrDryRun :: Lens.Lens' CancelSpotFleetRequests (Core.Maybe Core.Bool)
csfrDryRun = Lens.field @"dryRun"
{-# DEPRECATED csfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest CancelSpotFleetRequests where
  type Rs CancelSpotFleetRequests = CancelSpotFleetRequestsResponse
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
            ( Core.pure ("Action", "CancelSpotFleetRequests")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "SpotFleetRequestId" spotFleetRequestIds)
                Core.<> (Core.toQueryValue "TerminateInstances" terminateInstances)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotFleetRequestsResponse'
            Core.<$> ( x Core..@? "successfulFleetRequestSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> ( x Core..@? "unsuccessfulFleetRequestSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'mkCancelSpotFleetRequestsResponse' smart constructor.
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
  { -- | Information about the Spot Fleet requests that are successfully canceled.
    successfulFleetRequests :: Core.Maybe [Types.CancelSpotFleetRequestsSuccessItem],
    -- | Information about the Spot Fleet requests that are not successfully canceled.
    unsuccessfulFleetRequests :: Core.Maybe [Types.CancelSpotFleetRequestsErrorItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequestsResponse' value with any optional fields omitted.
mkCancelSpotFleetRequestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelSpotFleetRequestsResponse
mkCancelSpotFleetRequestsResponse responseStatus =
  CancelSpotFleetRequestsResponse'
    { successfulFleetRequests =
        Core.Nothing,
      unsuccessfulFleetRequests = Core.Nothing,
      responseStatus
    }

-- | Information about the Spot Fleet requests that are successfully canceled.
--
-- /Note:/ Consider using 'successfulFleetRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrrsSuccessfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Core.Maybe [Types.CancelSpotFleetRequestsSuccessItem])
csfrrrsSuccessfulFleetRequests = Lens.field @"successfulFleetRequests"
{-# DEPRECATED csfrrrsSuccessfulFleetRequests "Use generic-lens or generic-optics with 'successfulFleetRequests' instead." #-}

-- | Information about the Spot Fleet requests that are not successfully canceled.
--
-- /Note:/ Consider using 'unsuccessfulFleetRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrrsUnsuccessfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Core.Maybe [Types.CancelSpotFleetRequestsErrorItem])
csfrrrsUnsuccessfulFleetRequests = Lens.field @"unsuccessfulFleetRequests"
{-# DEPRECATED csfrrrsUnsuccessfulFleetRequests "Use generic-lens or generic-optics with 'unsuccessfulFleetRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrrsResponseStatus :: Lens.Lens' CancelSpotFleetRequestsResponse Core.Int
csfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
