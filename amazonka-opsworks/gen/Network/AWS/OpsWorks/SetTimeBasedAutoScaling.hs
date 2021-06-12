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
-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the time-based auto scaling configuration for a specified
-- instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
  ( -- * Creating a Request
    SetTimeBasedAutoScaling (..),
    newSetTimeBasedAutoScaling,

    -- * Request Lenses
    setTimeBasedAutoScaling_autoScalingSchedule,
    setTimeBasedAutoScaling_instanceId,

    -- * Destructuring the Response
    SetTimeBasedAutoScalingResponse (..),
    newSetTimeBasedAutoScalingResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetTimeBasedAutoScaling' smart constructor.
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
  { -- | An @AutoScalingSchedule@ with the instance schedule.
    autoScalingSchedule :: Core.Maybe WeeklyAutoScalingSchedule,
    -- | The instance ID.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetTimeBasedAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingSchedule', 'setTimeBasedAutoScaling_autoScalingSchedule' - An @AutoScalingSchedule@ with the instance schedule.
--
-- 'instanceId', 'setTimeBasedAutoScaling_instanceId' - The instance ID.
newSetTimeBasedAutoScaling ::
  -- | 'instanceId'
  Core.Text ->
  SetTimeBasedAutoScaling
newSetTimeBasedAutoScaling pInstanceId_ =
  SetTimeBasedAutoScaling'
    { autoScalingSchedule =
        Core.Nothing,
      instanceId = pInstanceId_
    }

-- | An @AutoScalingSchedule@ with the instance schedule.
setTimeBasedAutoScaling_autoScalingSchedule :: Lens.Lens' SetTimeBasedAutoScaling (Core.Maybe WeeklyAutoScalingSchedule)
setTimeBasedAutoScaling_autoScalingSchedule = Lens.lens (\SetTimeBasedAutoScaling' {autoScalingSchedule} -> autoScalingSchedule) (\s@SetTimeBasedAutoScaling' {} a -> s {autoScalingSchedule = a} :: SetTimeBasedAutoScaling)

-- | The instance ID.
setTimeBasedAutoScaling_instanceId :: Lens.Lens' SetTimeBasedAutoScaling Core.Text
setTimeBasedAutoScaling_instanceId = Lens.lens (\SetTimeBasedAutoScaling' {instanceId} -> instanceId) (\s@SetTimeBasedAutoScaling' {} a -> s {instanceId = a} :: SetTimeBasedAutoScaling)

instance Core.AWSRequest SetTimeBasedAutoScaling where
  type
    AWSResponse SetTimeBasedAutoScaling =
      SetTimeBasedAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      SetTimeBasedAutoScalingResponse'

instance Core.Hashable SetTimeBasedAutoScaling

instance Core.NFData SetTimeBasedAutoScaling

instance Core.ToHeaders SetTimeBasedAutoScaling where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.SetTimeBasedAutoScaling" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetTimeBasedAutoScaling where
  toJSON SetTimeBasedAutoScaling' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutoScalingSchedule" Core..=)
              Core.<$> autoScalingSchedule,
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath SetTimeBasedAutoScaling where
  toPath = Core.const "/"

instance Core.ToQuery SetTimeBasedAutoScaling where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetTimeBasedAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTimeBasedAutoScalingResponse ::
  SetTimeBasedAutoScalingResponse
newSetTimeBasedAutoScalingResponse =
  SetTimeBasedAutoScalingResponse'

instance Core.NFData SetTimeBasedAutoScalingResponse
