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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    scalingProcesses :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SuspendProcesses
newSuspendProcesses pAutoScalingGroupName_ =
  SuspendProcesses'
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
suspendProcesses_scalingProcesses :: Lens.Lens' SuspendProcesses (Prelude.Maybe [Prelude.Text])
suspendProcesses_scalingProcesses = Lens.lens (\SuspendProcesses' {scalingProcesses} -> scalingProcesses) (\s@SuspendProcesses' {} a -> s {scalingProcesses = a} :: SuspendProcesses) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Auto Scaling group.
suspendProcesses_autoScalingGroupName :: Lens.Lens' SuspendProcesses Prelude.Text
suspendProcesses_autoScalingGroupName = Lens.lens (\SuspendProcesses' {autoScalingGroupName} -> autoScalingGroupName) (\s@SuspendProcesses' {} a -> s {autoScalingGroupName = a} :: SuspendProcesses)

instance Prelude.AWSRequest SuspendProcesses where
  type Rs SuspendProcesses = SuspendProcessesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SuspendProcessesResponse'

instance Prelude.Hashable SuspendProcesses

instance Prelude.NFData SuspendProcesses

instance Prelude.ToHeaders SuspendProcesses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SuspendProcesses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SuspendProcesses where
  toQuery SuspendProcesses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SuspendProcesses" :: Prelude.ByteString),
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

-- | /See:/ 'newSuspendProcessesResponse' smart constructor.
data SuspendProcessesResponse = SuspendProcessesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuspendProcessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSuspendProcessesResponse ::
  SuspendProcessesResponse
newSuspendProcessesResponse =
  SuspendProcessesResponse'

instance Prelude.NFData SuspendProcessesResponse
