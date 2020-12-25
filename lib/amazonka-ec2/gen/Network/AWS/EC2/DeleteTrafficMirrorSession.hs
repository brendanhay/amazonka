{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorSession
  ( -- * Creating a request
    DeleteTrafficMirrorSession (..),
    mkDeleteTrafficMirrorSession,

    -- ** Request lenses
    dtmsfTrafficMirrorSessionId,
    dtmsfDryRun,

    -- * Destructuring the response
    DeleteTrafficMirrorSessionResponse (..),
    mkDeleteTrafficMirrorSessionResponse,

    -- ** Response lenses
    dtmsrfrsTrafficMirrorSessionId,
    dtmsrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrafficMirrorSession' smart constructor.
data DeleteTrafficMirrorSession = DeleteTrafficMirrorSession'
  { -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Types.TrafficMirrorSessionId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorSession' value with any optional fields omitted.
mkDeleteTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Types.TrafficMirrorSessionId ->
  DeleteTrafficMirrorSession
mkDeleteTrafficMirrorSession trafficMirrorSessionId =
  DeleteTrafficMirrorSession'
    { trafficMirrorSessionId,
      dryRun = Core.Nothing
    }

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsfTrafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSession Types.TrafficMirrorSessionId
dtmsfTrafficMirrorSessionId = Lens.field @"trafficMirrorSessionId"
{-# DEPRECATED dtmsfTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsfDryRun :: Lens.Lens' DeleteTrafficMirrorSession (Core.Maybe Core.Bool)
dtmsfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTrafficMirrorSession where
  type
    Rs DeleteTrafficMirrorSession =
      DeleteTrafficMirrorSessionResponse
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
            ( Core.pure ("Action", "DeleteTrafficMirrorSession")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorSessionId" trafficMirrorSessionId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorSessionResponse'
            Core.<$> (x Core..@? "trafficMirrorSessionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrafficMirrorSessionResponse' smart constructor.
data DeleteTrafficMirrorSessionResponse = DeleteTrafficMirrorSessionResponse'
  { -- | The ID of the deleted Traffic Mirror session.
    trafficMirrorSessionId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorSessionResponse' value with any optional fields omitted.
mkDeleteTrafficMirrorSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrafficMirrorSessionResponse
mkDeleteTrafficMirrorSessionResponse responseStatus =
  DeleteTrafficMirrorSessionResponse'
    { trafficMirrorSessionId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the deleted Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsrfrsTrafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSessionResponse (Core.Maybe Types.String)
dtmsrfrsTrafficMirrorSessionId = Lens.field @"trafficMirrorSessionId"
{-# DEPRECATED dtmsrfrsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsrfrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorSessionResponse Core.Int
dtmsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
