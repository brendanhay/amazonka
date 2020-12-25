{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorFilter
  ( -- * Creating a request
    DeleteTrafficMirrorFilter (..),
    mkDeleteTrafficMirrorFilter,

    -- ** Request lenses
    dtmffTrafficMirrorFilterId,
    dtmffDryRun,

    -- * Destructuring the response
    DeleteTrafficMirrorFilterResponse (..),
    mkDeleteTrafficMirrorFilterResponse,

    -- ** Response lenses
    dtmfrfrsTrafficMirrorFilterId,
    dtmfrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Types.TrafficMirrorFilterId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilter' value with any optional fields omitted.
mkDeleteTrafficMirrorFilter ::
  -- | 'trafficMirrorFilterId'
  Types.TrafficMirrorFilterId ->
  DeleteTrafficMirrorFilter
mkDeleteTrafficMirrorFilter trafficMirrorFilterId =
  DeleteTrafficMirrorFilter'
    { trafficMirrorFilterId,
      dryRun = Core.Nothing
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmffTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilter Types.TrafficMirrorFilterId
dtmffTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED dtmffTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmffDryRun :: Lens.Lens' DeleteTrafficMirrorFilter (Core.Maybe Core.Bool)
dtmffDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmffDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTrafficMirrorFilter where
  type
    Rs DeleteTrafficMirrorFilter =
      DeleteTrafficMirrorFilterResponse
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
            ( Core.pure ("Action", "DeleteTrafficMirrorFilter")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorFilterId" trafficMirrorFilterId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterResponse'
            Core.<$> (x Core..@? "trafficMirrorFilterId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilterResponse' value with any optional fields omitted.
mkDeleteTrafficMirrorFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrafficMirrorFilterResponse
mkDeleteTrafficMirrorFilterResponse responseStatus =
  DeleteTrafficMirrorFilterResponse'
    { trafficMirrorFilterId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrfrsTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilterResponse (Core.Maybe Types.String)
dtmfrfrsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED dtmfrfrsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrfrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorFilterResponse Core.Int
dtmfrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmfrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
