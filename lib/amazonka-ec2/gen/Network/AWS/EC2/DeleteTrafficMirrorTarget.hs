{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror target.
--
-- You cannot delete a Traffic Mirror target that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorTarget
  ( -- * Creating a request
    DeleteTrafficMirrorTarget (..),
    mkDeleteTrafficMirrorTarget,

    -- ** Request lenses
    dtmtfTrafficMirrorTargetId,
    dtmtfDryRun,

    -- * Destructuring the response
    DeleteTrafficMirrorTargetResponse (..),
    mkDeleteTrafficMirrorTargetResponse,

    -- ** Response lenses
    dtmtrfrsTrafficMirrorTargetId,
    dtmtrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrafficMirrorTarget' smart constructor.
data DeleteTrafficMirrorTarget = DeleteTrafficMirrorTarget'
  { -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Types.TrafficMirrorTargetId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorTarget' value with any optional fields omitted.
mkDeleteTrafficMirrorTarget ::
  -- | 'trafficMirrorTargetId'
  Types.TrafficMirrorTargetId ->
  DeleteTrafficMirrorTarget
mkDeleteTrafficMirrorTarget trafficMirrorTargetId =
  DeleteTrafficMirrorTarget'
    { trafficMirrorTargetId,
      dryRun = Core.Nothing
    }

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtfTrafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTarget Types.TrafficMirrorTargetId
dtmtfTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# DEPRECATED dtmtfTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtfDryRun :: Lens.Lens' DeleteTrafficMirrorTarget (Core.Maybe Core.Bool)
dtmtfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmtfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTrafficMirrorTarget where
  type
    Rs DeleteTrafficMirrorTarget =
      DeleteTrafficMirrorTargetResponse
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
            ( Core.pure ("Action", "DeleteTrafficMirrorTarget")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorTargetId" trafficMirrorTargetId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorTargetResponse'
            Core.<$> (x Core..@? "trafficMirrorTargetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'
  { -- | The ID of the deleted Traffic Mirror target.
    trafficMirrorTargetId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorTargetResponse' value with any optional fields omitted.
mkDeleteTrafficMirrorTargetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrafficMirrorTargetResponse
mkDeleteTrafficMirrorTargetResponse responseStatus =
  DeleteTrafficMirrorTargetResponse'
    { trafficMirrorTargetId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the deleted Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrfrsTrafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTargetResponse (Core.Maybe Types.String)
dtmtrfrsTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# DEPRECATED dtmtrfrsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrfrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorTargetResponse Core.Int
dtmtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
