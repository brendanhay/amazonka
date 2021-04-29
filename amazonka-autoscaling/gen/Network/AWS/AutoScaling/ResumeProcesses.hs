{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    scalingProcesses :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ResumeProcesses
newResumeProcesses pAutoScalingGroupName_ =
  ResumeProcesses'
    { scalingProcesses =
        Prelude.Nothing,
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
resumeProcesses_scalingProcesses :: Lens.Lens' ResumeProcesses (Prelude.Maybe [Prelude.Text])
resumeProcesses_scalingProcesses = Lens.lens (\ResumeProcesses' {scalingProcesses} -> scalingProcesses) (\s@ResumeProcesses' {} a -> s {scalingProcesses = a} :: ResumeProcesses) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Auto Scaling group.
resumeProcesses_autoScalingGroupName :: Lens.Lens' ResumeProcesses Prelude.Text
resumeProcesses_autoScalingGroupName = Lens.lens (\ResumeProcesses' {autoScalingGroupName} -> autoScalingGroupName) (\s@ResumeProcesses' {} a -> s {autoScalingGroupName = a} :: ResumeProcesses)

instance Prelude.AWSRequest ResumeProcesses where
  type Rs ResumeProcesses = ResumeProcessesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ResumeProcessesResponse'

instance Prelude.Hashable ResumeProcesses

instance Prelude.NFData ResumeProcesses

instance Prelude.ToHeaders ResumeProcesses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResumeProcesses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResumeProcesses where
  toQuery ResumeProcesses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResumeProcesses" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "ScalingProcesses"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> scalingProcesses
            ),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newResumeProcessesResponse' smart constructor.
data ResumeProcessesResponse = ResumeProcessesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResumeProcessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResumeProcessesResponse ::
  ResumeProcessesResponse
newResumeProcessesResponse = ResumeProcessesResponse'

instance Prelude.NFData ResumeProcessesResponse
