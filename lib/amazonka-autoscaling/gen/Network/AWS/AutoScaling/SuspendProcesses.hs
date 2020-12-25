{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SuspendProcesses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the specified auto scaling processes, or all processes, for the specified Auto Scaling group.
--
-- If you suspend either the @Launch@ or @Terminate@ process types, it can prevent other process types from functioning properly. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html Suspending and resuming scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
-- To resume processes that have been suspended, call the 'ResumeProcesses' API.
module Network.AWS.AutoScaling.SuspendProcesses
  ( -- * Creating a request
    SuspendProcesses (..),
    mkSuspendProcesses,

    -- ** Request lenses
    sAutoScalingGroupName,
    sScalingProcesses,

    -- * Destructuring the response
    SuspendProcessesResponse (..),
    mkSuspendProcessesResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSuspendProcesses' smart constructor.
data SuspendProcesses = SuspendProcesses'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | One or more of the following processes:
    --
    --
    --     * @Launch@
    --
    --
    --     * @Terminate@
    --
    --
    --     * @AddToLoadBalancer@
    --
    --
    --     * @AlarmNotification@
    --
    --
    --     * @AZRebalance@
    --
    --
    --     * @HealthCheck@
    --
    --
    --     * @InstanceRefresh@
    --
    --
    --     * @ReplaceUnhealthy@
    --
    --
    --     * @ScheduledActions@
    --
    --
    -- If you omit this parameter, all processes are specified.
    scalingProcesses :: Core.Maybe [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendProcesses' value with any optional fields omitted.
mkSuspendProcesses ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  SuspendProcesses
mkSuspendProcesses autoScalingGroupName =
  SuspendProcesses'
    { autoScalingGroupName,
      scalingProcesses = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoScalingGroupName :: Lens.Lens' SuspendProcesses Types.ResourceName
sAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED sAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | One or more of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
--
--
-- If you omit this parameter, all processes are specified.
--
-- /Note:/ Consider using 'scalingProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScalingProcesses :: Lens.Lens' SuspendProcesses (Core.Maybe [Types.XmlStringMaxLen255])
sScalingProcesses = Lens.field @"scalingProcesses"
{-# DEPRECATED sScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}

instance Core.AWSRequest SuspendProcesses where
  type Rs SuspendProcesses = SuspendProcessesResponse
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
            ( Core.pure ("Action", "SuspendProcesses")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "ScalingProcesses"
                            (Core.toQueryList "member" Core.<$> scalingProcesses)
                        )
            )
      }
  response = Response.receiveNull SuspendProcessesResponse'

-- | /See:/ 'mkSuspendProcessesResponse' smart constructor.
data SuspendProcessesResponse = SuspendProcessesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendProcessesResponse' value with any optional fields omitted.
mkSuspendProcessesResponse ::
  SuspendProcessesResponse
mkSuspendProcessesResponse = SuspendProcessesResponse'
