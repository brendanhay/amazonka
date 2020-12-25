{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror rule.
module Network.AWS.EC2.DeleteTrafficMirrorFilterRule
  ( -- * Creating a request
    DeleteTrafficMirrorFilterRule (..),
    mkDeleteTrafficMirrorFilterRule,

    -- ** Request lenses
    dtmfrTrafficMirrorFilterRuleId,
    dtmfrDryRun,

    -- * Destructuring the response
    DeleteTrafficMirrorFilterRuleResponse (..),
    mkDeleteTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    dtmfrrrsTrafficMirrorFilterRuleId,
    dtmfrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrafficMirrorFilterRule' smart constructor.
data DeleteTrafficMirrorFilterRule = DeleteTrafficMirrorFilterRule'
  { -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Types.TrafficMirrorFilterRuleId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilterRule' value with any optional fields omitted.
mkDeleteTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Types.TrafficMirrorFilterRuleId ->
  DeleteTrafficMirrorFilterRule
mkDeleteTrafficMirrorFilterRule trafficMirrorFilterRuleId =
  DeleteTrafficMirrorFilterRule'
    { trafficMirrorFilterRuleId,
      dryRun = Core.Nothing
    }

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrTrafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRule Types.TrafficMirrorFilterRuleId
dtmfrTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# DEPRECATED dtmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrDryRun :: Lens.Lens' DeleteTrafficMirrorFilterRule (Core.Maybe Core.Bool)
dtmfrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTrafficMirrorFilterRule where
  type
    Rs DeleteTrafficMirrorFilterRule =
      DeleteTrafficMirrorFilterRuleResponse
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
            ( Core.pure ("Action", "DeleteTrafficMirrorFilterRule")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TrafficMirrorFilterRuleId"
                            trafficMirrorFilterRuleId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterRuleResponse'
            Core.<$> (x Core..@? "trafficMirrorFilterRuleId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrafficMirrorFilterRuleResponse' smart constructor.
data DeleteTrafficMirrorFilterRuleResponse = DeleteTrafficMirrorFilterRuleResponse'
  { -- | The ID of the deleted Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Core.Maybe Types.TrafficMirrorFilterRuleId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilterRuleResponse' value with any optional fields omitted.
mkDeleteTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrafficMirrorFilterRuleResponse
mkDeleteTrafficMirrorFilterRuleResponse responseStatus =
  DeleteTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRuleId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the deleted Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrrsTrafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse (Core.Maybe Types.TrafficMirrorFilterRuleId)
dtmfrrrsTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# DEPRECATED dtmfrrrsTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse Core.Int
dtmfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
