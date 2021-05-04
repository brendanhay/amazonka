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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetTimeBasedAutoScaling' smart constructor.
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
  { -- | An @AutoScalingSchedule@ with the instance schedule.
    autoScalingSchedule :: Prelude.Maybe WeeklyAutoScalingSchedule,
    -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SetTimeBasedAutoScaling
newSetTimeBasedAutoScaling pInstanceId_ =
  SetTimeBasedAutoScaling'
    { autoScalingSchedule =
        Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | An @AutoScalingSchedule@ with the instance schedule.
setTimeBasedAutoScaling_autoScalingSchedule :: Lens.Lens' SetTimeBasedAutoScaling (Prelude.Maybe WeeklyAutoScalingSchedule)
setTimeBasedAutoScaling_autoScalingSchedule = Lens.lens (\SetTimeBasedAutoScaling' {autoScalingSchedule} -> autoScalingSchedule) (\s@SetTimeBasedAutoScaling' {} a -> s {autoScalingSchedule = a} :: SetTimeBasedAutoScaling)

-- | The instance ID.
setTimeBasedAutoScaling_instanceId :: Lens.Lens' SetTimeBasedAutoScaling Prelude.Text
setTimeBasedAutoScaling_instanceId = Lens.lens (\SetTimeBasedAutoScaling' {instanceId} -> instanceId) (\s@SetTimeBasedAutoScaling' {} a -> s {instanceId = a} :: SetTimeBasedAutoScaling)

instance Prelude.AWSRequest SetTimeBasedAutoScaling where
  type
    Rs SetTimeBasedAutoScaling =
      SetTimeBasedAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      SetTimeBasedAutoScalingResponse'

instance Prelude.Hashable SetTimeBasedAutoScaling

instance Prelude.NFData SetTimeBasedAutoScaling

instance Prelude.ToHeaders SetTimeBasedAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.SetTimeBasedAutoScaling" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetTimeBasedAutoScaling where
  toJSON SetTimeBasedAutoScaling' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AutoScalingSchedule" Prelude..=)
              Prelude.<$> autoScalingSchedule,
            Prelude.Just ("InstanceId" Prelude..= instanceId)
          ]
      )

instance Prelude.ToPath SetTimeBasedAutoScaling where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetTimeBasedAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetTimeBasedAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTimeBasedAutoScalingResponse ::
  SetTimeBasedAutoScalingResponse
newSetTimeBasedAutoScalingResponse =
  SetTimeBasedAutoScalingResponse'

instance
  Prelude.NFData
    SetTimeBasedAutoScalingResponse
