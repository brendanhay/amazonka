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
-- Module      : Amazonka.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.AutoScaling.ResumeProcesses
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- If you omit this property, all processes are specified.
    scalingProcesses :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- If you omit this property, all processes are specified.
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
-- If you omit this property, all processes are specified.
resumeProcesses_scalingProcesses :: Lens.Lens' ResumeProcesses (Prelude.Maybe [Prelude.Text])
resumeProcesses_scalingProcesses = Lens.lens (\ResumeProcesses' {scalingProcesses} -> scalingProcesses) (\s@ResumeProcesses' {} a -> s {scalingProcesses = a} :: ResumeProcesses) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
resumeProcesses_autoScalingGroupName :: Lens.Lens' ResumeProcesses Prelude.Text
resumeProcesses_autoScalingGroupName = Lens.lens (\ResumeProcesses' {autoScalingGroupName} -> autoScalingGroupName) (\s@ResumeProcesses' {} a -> s {autoScalingGroupName = a} :: ResumeProcesses)

instance Core.AWSRequest ResumeProcesses where
  type
    AWSResponse ResumeProcesses =
      ResumeProcessesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ResumeProcessesResponse'

instance Prelude.Hashable ResumeProcesses where
  hashWithSalt _salt ResumeProcesses' {..} =
    _salt
      `Prelude.hashWithSalt` scalingProcesses
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData ResumeProcesses where
  rnf ResumeProcesses' {..} =
    Prelude.rnf scalingProcesses
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders ResumeProcesses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResumeProcesses where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeProcesses where
  toQuery ResumeProcesses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResumeProcesses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "ScalingProcesses"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> scalingProcesses
            ),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newResumeProcessesResponse' smart constructor.
data ResumeProcessesResponse = ResumeProcessesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeProcessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResumeProcessesResponse ::
  ResumeProcessesResponse
newResumeProcessesResponse = ResumeProcessesResponse'

instance Prelude.NFData ResumeProcessesResponse where
  rnf _ = ()
