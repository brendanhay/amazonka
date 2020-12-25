{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables detailed monitoring for a running instance. Otherwise, basic monitoring is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- To disable detailed monitoring, see .
module Network.AWS.EC2.MonitorInstances
  ( -- * Creating a request
    MonitorInstances (..),
    mkMonitorInstances,

    -- ** Request lenses
    miInstanceIds,
    miDryRun,

    -- * Destructuring the response
    MonitorInstancesResponse (..),
    mkMonitorInstancesResponse,

    -- ** Response lenses
    mirrsInstanceMonitorings,
    mirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMonitorInstances' smart constructor.
data MonitorInstances = MonitorInstances'
  { -- | The IDs of the instances.
    instanceIds :: [Types.InstanceId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitorInstances' value with any optional fields omitted.
mkMonitorInstances ::
  MonitorInstances
mkMonitorInstances =
  MonitorInstances'
    { instanceIds = Core.mempty,
      dryRun = Core.Nothing
    }

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miInstanceIds :: Lens.Lens' MonitorInstances [Types.InstanceId]
miInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED miInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miDryRun :: Lens.Lens' MonitorInstances (Core.Maybe Core.Bool)
miDryRun = Lens.field @"dryRun"
{-# DEPRECATED miDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest MonitorInstances where
  type Rs MonitorInstances = MonitorInstancesResponse
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
            ( Core.pure ("Action", "MonitorInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "InstanceId" instanceIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          MonitorInstancesResponse'
            Core.<$> (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkMonitorInstancesResponse' smart constructor.
data MonitorInstancesResponse = MonitorInstancesResponse'
  { -- | The monitoring information.
    instanceMonitorings :: Core.Maybe [Types.InstanceMonitoring],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitorInstancesResponse' value with any optional fields omitted.
mkMonitorInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  MonitorInstancesResponse
mkMonitorInstancesResponse responseStatus =
  MonitorInstancesResponse'
    { instanceMonitorings = Core.Nothing,
      responseStatus
    }

-- | The monitoring information.
--
-- /Note:/ Consider using 'instanceMonitorings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mirrsInstanceMonitorings :: Lens.Lens' MonitorInstancesResponse (Core.Maybe [Types.InstanceMonitoring])
mirrsInstanceMonitorings = Lens.field @"instanceMonitorings"
{-# DEPRECATED mirrsInstanceMonitorings "Use generic-lens or generic-optics with 'instanceMonitorings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mirrsResponseStatus :: Lens.Lens' MonitorInstancesResponse Core.Int
mirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
