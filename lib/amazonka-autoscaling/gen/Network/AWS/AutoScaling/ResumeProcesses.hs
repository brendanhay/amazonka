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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResumeProcesses' smart constructor.
data ResumeProcesses = ResumeProcesses'
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

-- | Creates a value of 'ResumeProcesses' with the minimum fields required to make a request.
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
mkResumeProcesses ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  ResumeProcesses
mkResumeProcesses pAutoScalingGroupName_ =
  ResumeProcesses'
    { autoScalingGroupName = pAutoScalingGroupName_,
      scalingProcesses = Lude.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpAutoScalingGroupName :: Lens.Lens' ResumeProcesses Lude.Text
rpAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ResumeProcesses -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ResumeProcesses)
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
rpScalingProcesses :: Lens.Lens' ResumeProcesses (Lude.Maybe [Lude.Text])
rpScalingProcesses = Lens.lens (scalingProcesses :: ResumeProcesses -> Lude.Maybe [Lude.Text]) (\s a -> s {scalingProcesses = a} :: ResumeProcesses)
{-# DEPRECATED rpScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}

instance Lude.AWSRequest ResumeProcesses where
  type Rs ResumeProcesses = ResumeProcessesResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull ResumeProcessesResponse'

instance Lude.ToHeaders ResumeProcesses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResumeProcesses where
  toPath = Lude.const "/"

instance Lude.ToQuery ResumeProcesses where
  toQuery ResumeProcesses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResumeProcesses" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScalingProcesses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> scalingProcesses)
      ]

-- | /See:/ 'mkResumeProcessesResponse' smart constructor.
data ResumeProcessesResponse = ResumeProcessesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeProcessesResponse' with the minimum fields required to make a request.
mkResumeProcessesResponse ::
  ResumeProcessesResponse
mkResumeProcessesResponse = ResumeProcessesResponse'
