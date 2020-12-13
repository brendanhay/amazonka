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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSuspendProcesses' smart constructor.
data SuspendProcesses = SuspendProcesses'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
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
    scalingProcesses :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendProcesses' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scalingProcesses' - One or more of the following processes:
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
mkSuspendProcesses ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  SuspendProcesses
mkSuspendProcesses pAutoScalingGroupName_ =
  SuspendProcesses'
    { autoScalingGroupName = pAutoScalingGroupName_,
      scalingProcesses = Lude.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoScalingGroupName :: Lens.Lens' SuspendProcesses Lude.Text
sAutoScalingGroupName = Lens.lens (autoScalingGroupName :: SuspendProcesses -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: SuspendProcesses)
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
sScalingProcesses :: Lens.Lens' SuspendProcesses (Lude.Maybe [Lude.Text])
sScalingProcesses = Lens.lens (scalingProcesses :: SuspendProcesses -> Lude.Maybe [Lude.Text]) (\s a -> s {scalingProcesses = a} :: SuspendProcesses)
{-# DEPRECATED sScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}

instance Lude.AWSRequest SuspendProcesses where
  type Rs SuspendProcesses = SuspendProcessesResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull SuspendProcessesResponse'

instance Lude.ToHeaders SuspendProcesses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SuspendProcesses where
  toPath = Lude.const "/"

instance Lude.ToQuery SuspendProcesses where
  toQuery SuspendProcesses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SuspendProcesses" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScalingProcesses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> scalingProcesses)
      ]

-- | /See:/ 'mkSuspendProcessesResponse' smart constructor.
data SuspendProcessesResponse = SuspendProcessesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendProcessesResponse' with the minimum fields required to make a request.
mkSuspendProcessesResponse ::
  SuspendProcessesResponse
mkSuspendProcessesResponse = SuspendProcessesResponse'
