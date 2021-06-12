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
-- Module      : Network.AWS.AutoScaling.SuspendProcesses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the specified auto scaling processes, or all processes, for the
-- specified Auto Scaling group.
--
-- If you suspend either the @Launch@ or @Terminate@ process types, it can
-- prevent other process types from functioning properly. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html Suspending and resuming scaling processes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- To resume processes that have been suspended, call the ResumeProcesses
-- API.
module Network.AWS.AutoScaling.SuspendProcesses
  ( -- * Creating a Request
    SuspendProcesses (..),
    newSuspendProcesses,

    -- * Request Lenses
    suspendProcesses_scalingProcesses,
    suspendProcesses_autoScalingGroupName,

    -- * Destructuring the Response
    SuspendProcessesResponse (..),
    newSuspendProcessesResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSuspendProcesses' smart constructor.
data SuspendProcesses = SuspendProcesses'
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
-- Create a value of 'SuspendProcesses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingProcesses', 'suspendProcesses_scalingProcesses' - One or more of the following processes:
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
-- 'autoScalingGroupName', 'suspendProcesses_autoScalingGroupName' - The name of the Auto Scaling group.
newSuspendProcesses ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  SuspendProcesses
newSuspendProcesses pAutoScalingGroupName_ =
  SuspendProcesses'
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
suspendProcesses_scalingProcesses :: Lens.Lens' SuspendProcesses (Core.Maybe [Core.Text])
suspendProcesses_scalingProcesses = Lens.lens (\SuspendProcesses' {scalingProcesses} -> scalingProcesses) (\s@SuspendProcesses' {} a -> s {scalingProcesses = a} :: SuspendProcesses) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
suspendProcesses_autoScalingGroupName :: Lens.Lens' SuspendProcesses Core.Text
suspendProcesses_autoScalingGroupName = Lens.lens (\SuspendProcesses' {autoScalingGroupName} -> autoScalingGroupName) (\s@SuspendProcesses' {} a -> s {autoScalingGroupName = a} :: SuspendProcesses)

instance Core.AWSRequest SuspendProcesses where
  type
    AWSResponse SuspendProcesses =
      SuspendProcessesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SuspendProcessesResponse'

instance Core.Hashable SuspendProcesses

instance Core.NFData SuspendProcesses

instance Core.ToHeaders SuspendProcesses where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SuspendProcesses where
  toPath = Core.const "/"

instance Core.ToQuery SuspendProcesses where
  toQuery SuspendProcesses' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SuspendProcesses" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "ScalingProcesses"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> scalingProcesses
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newSuspendProcessesResponse' smart constructor.
data SuspendProcessesResponse = SuspendProcessesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SuspendProcessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSuspendProcessesResponse ::
  SuspendProcessesResponse
newSuspendProcessesResponse =
  SuspendProcessesResponse'

instance Core.NFData SuspendProcessesResponse
