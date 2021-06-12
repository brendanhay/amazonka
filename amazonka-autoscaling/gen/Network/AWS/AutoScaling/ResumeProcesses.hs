{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the specified suspended auto scaling processes, or all suspended
-- process, for the specified Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html Suspending and resuming scaling processes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.ResumeProcesses
  ( -- * Creating a Request
    ResumeProcesses (..),
    newResumeProcesses,

    -- * Request Lenses
    resumeProcesses_scalingProcesses,
    resumeProcesses_autoScalingGroupName,

    -- * Destructuring the Response
    ResumeProcessesResponse (..),
    newResumeProcessesResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResumeProcesses' smart constructor.
data ResumeProcesses = ResumeProcesses'
  { -- | One or more of the following processes:
    --
    -- -   @Launch@
    --
    -- -   @Terminate@
    --
    -- -   @AddToLoadBalancer@
    --
    -- -   @AlarmNotification@
    --
    -- -   @AZRebalance@
    --
    -- -   @HealthCheck@
    --
    -- -   @InstanceRefresh@
    --
    -- -   @ReplaceUnhealthy@
    --
    -- -   @ScheduledActions@
    --
    -- If you omit this parameter, all processes are specified.
    scalingProcesses :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResumeProcesses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingProcesses', 'resumeProcesses_scalingProcesses' - One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @AddToLoadBalancer@
--
-- -   @AlarmNotification@
--
-- -   @AZRebalance@
--
-- -   @HealthCheck@
--
-- -   @InstanceRefresh@
--
-- -   @ReplaceUnhealthy@
--
-- -   @ScheduledActions@
--
-- If you omit this parameter, all processes are specified.
--
-- 'autoScalingGroupName', 'resumeProcesses_autoScalingGroupName' - The name of the Auto Scaling group.
newResumeProcesses ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  ResumeProcesses
newResumeProcesses pAutoScalingGroupName_ =
  ResumeProcesses'
    { scalingProcesses = Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @AddToLoadBalancer@
--
-- -   @AlarmNotification@
--
-- -   @AZRebalance@
--
-- -   @HealthCheck@
--
-- -   @InstanceRefresh@
--
-- -   @ReplaceUnhealthy@
--
-- -   @ScheduledActions@
--
-- If you omit this parameter, all processes are specified.
resumeProcesses_scalingProcesses :: Lens.Lens' ResumeProcesses (Core.Maybe [Core.Text])
resumeProcesses_scalingProcesses = Lens.lens (\ResumeProcesses' {scalingProcesses} -> scalingProcesses) (\s@ResumeProcesses' {} a -> s {scalingProcesses = a} :: ResumeProcesses) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
resumeProcesses_autoScalingGroupName :: Lens.Lens' ResumeProcesses Core.Text
resumeProcesses_autoScalingGroupName = Lens.lens (\ResumeProcesses' {autoScalingGroupName} -> autoScalingGroupName) (\s@ResumeProcesses' {} a -> s {autoScalingGroupName = a} :: ResumeProcesses)

instance Core.AWSRequest ResumeProcesses where
  type
    AWSResponse ResumeProcesses =
      ResumeProcessesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ResumeProcessesResponse'

instance Core.Hashable ResumeProcesses

instance Core.NFData ResumeProcesses

instance Core.ToHeaders ResumeProcesses where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ResumeProcesses where
  toPath = Core.const "/"

instance Core.ToQuery ResumeProcesses where
  toQuery ResumeProcesses' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ResumeProcesses" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "ScalingProcesses"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> scalingProcesses
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newResumeProcessesResponse' smart constructor.
data ResumeProcessesResponse = ResumeProcessesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResumeProcessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResumeProcessesResponse ::
  ResumeProcessesResponse
newResumeProcessesResponse = ResumeProcessesResponse'

instance Core.NFData ResumeProcessesResponse
