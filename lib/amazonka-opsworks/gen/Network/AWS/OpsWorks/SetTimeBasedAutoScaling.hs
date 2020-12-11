{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the time-based auto scaling configuration for a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
  ( -- * Creating a request
    SetTimeBasedAutoScaling (..),
    mkSetTimeBasedAutoScaling,

    -- ** Request lenses
    stbasAutoScalingSchedule,
    stbasInstanceId,

    -- * Destructuring the response
    SetTimeBasedAutoScalingResponse (..),
    mkSetTimeBasedAutoScalingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetTimeBasedAutoScaling' smart constructor.
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
  { autoScalingSchedule ::
      Lude.Maybe WeeklyAutoScalingSchedule,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTimeBasedAutoScaling' with the minimum fields required to make a request.
--
-- * 'autoScalingSchedule' - An @AutoScalingSchedule@ with the instance schedule.
-- * 'instanceId' - The instance ID.
mkSetTimeBasedAutoScaling ::
  -- | 'instanceId'
  Lude.Text ->
  SetTimeBasedAutoScaling
mkSetTimeBasedAutoScaling pInstanceId_ =
  SetTimeBasedAutoScaling'
    { autoScalingSchedule = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | An @AutoScalingSchedule@ with the instance schedule.
--
-- /Note:/ Consider using 'autoScalingSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stbasAutoScalingSchedule :: Lens.Lens' SetTimeBasedAutoScaling (Lude.Maybe WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule = Lens.lens (autoScalingSchedule :: SetTimeBasedAutoScaling -> Lude.Maybe WeeklyAutoScalingSchedule) (\s a -> s {autoScalingSchedule = a} :: SetTimeBasedAutoScaling)
{-# DEPRECATED stbasAutoScalingSchedule "Use generic-lens or generic-optics with 'autoScalingSchedule' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stbasInstanceId :: Lens.Lens' SetTimeBasedAutoScaling Lude.Text
stbasInstanceId = Lens.lens (instanceId :: SetTimeBasedAutoScaling -> Lude.Text) (\s a -> s {instanceId = a} :: SetTimeBasedAutoScaling)
{-# DEPRECATED stbasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest SetTimeBasedAutoScaling where
  type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull SetTimeBasedAutoScalingResponse'

instance Lude.ToHeaders SetTimeBasedAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.SetTimeBasedAutoScaling" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetTimeBasedAutoScaling where
  toJSON SetTimeBasedAutoScaling' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoScalingSchedule" Lude..=) Lude.<$> autoScalingSchedule,
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath SetTimeBasedAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTimeBasedAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTimeBasedAutoScalingResponse' with the minimum fields required to make a request.
mkSetTimeBasedAutoScalingResponse ::
  SetTimeBasedAutoScalingResponse
mkSetTimeBasedAutoScalingResponse =
  SetTimeBasedAutoScalingResponse'
