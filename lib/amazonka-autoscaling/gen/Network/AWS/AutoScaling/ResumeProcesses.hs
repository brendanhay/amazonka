{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the specified suspended auto scaling processes, or all suspended process, for the specified Auto Scaling group.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html Suspending and resuming scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.ResumeProcesses
  ( -- * Creating a request
    ResumeProcesses (..),
    mkResumeProcesses,

    -- ** Request lenses
    rpAutoScalingGroupName,
    rpScalingProcesses,

    -- * Destructuring the response
    ResumeProcessesResponse (..),
    mkResumeProcessesResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeProcesses' smart constructor.
data ResumeProcesses = ResumeProcesses'
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

-- | Creates a 'ResumeProcesses' value with any optional fields omitted.
mkResumeProcesses ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  ResumeProcesses
mkResumeProcesses autoScalingGroupName =
  ResumeProcesses'
    { autoScalingGroupName,
      scalingProcesses = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpAutoScalingGroupName :: Lens.Lens' ResumeProcesses Types.ResourceName
rpAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED rpAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

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
rpScalingProcesses :: Lens.Lens' ResumeProcesses (Core.Maybe [Types.XmlStringMaxLen255])
rpScalingProcesses = Lens.field @"scalingProcesses"
{-# DEPRECATED rpScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}

instance Core.AWSRequest ResumeProcesses where
  type Rs ResumeProcesses = ResumeProcessesResponse
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
            ( Core.pure ("Action", "ResumeProcesses")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "ScalingProcesses"
                            (Core.toQueryList "member" Core.<$> scalingProcesses)
                        )
            )
      }
  response = Response.receiveNull ResumeProcessesResponse'

-- | /See:/ 'mkResumeProcessesResponse' smart constructor.
data ResumeProcessesResponse = ResumeProcessesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeProcessesResponse' value with any optional fields omitted.
mkResumeProcessesResponse ::
  ResumeProcessesResponse
mkResumeProcessesResponse = ResumeProcessesResponse'
