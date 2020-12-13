{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Temporarily sets the state of an alarm for testing purposes. When the updated state differs from the previous value, the action configured for the appropriate state is invoked. For example, if your alarm is configured to send an Amazon SNS message when an alarm is triggered, temporarily changing the alarm state to @ALARM@ sends an SNS message.
--
-- Metric alarms returns to their actual state quickly, often within seconds. Because the metric alarm state change happens quickly, it is typically only visible in the alarm's __History__ tab in the Amazon CloudWatch console or through <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html DescribeAlarmHistory> .
-- If you use @SetAlarmState@ on a composite alarm, the composite alarm is not guaranteed to return to its actual state. It returns to its actual state only once any of its children alarms change state. It is also reevaluated if you update its configuration.
-- If an alarm triggers EC2 Auto Scaling policies or application Auto Scaling policies, you must include information in the @StateReasonData@ parameter to enable the policy to take the correct action.
module Network.AWS.CloudWatch.SetAlarmState
  ( -- * Creating a request
    SetAlarmState (..),
    mkSetAlarmState,

    -- ** Request lenses
    sasAlarmName,
    sasStateValue,
    sasStateReason,
    sasStateReasonData,

    -- * Destructuring the response
    SetAlarmStateResponse (..),
    mkSetAlarmStateResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetAlarmState' smart constructor.
data SetAlarmState = SetAlarmState'
  { -- | The name of the alarm.
    alarmName :: Lude.Text,
    -- | The value of the state.
    stateValue :: StateValue,
    -- | The reason that this alarm is set to this specific state, in text format.
    stateReason :: Lude.Text,
    -- | The reason that this alarm is set to this specific state, in JSON format.
    --
    -- For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
    stateReasonData :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetAlarmState' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name of the alarm.
-- * 'stateValue' - The value of the state.
-- * 'stateReason' - The reason that this alarm is set to this specific state, in text format.
-- * 'stateReasonData' - The reason that this alarm is set to this specific state, in JSON format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
mkSetAlarmState ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'stateValue'
  StateValue ->
  -- | 'stateReason'
  Lude.Text ->
  SetAlarmState
mkSetAlarmState pAlarmName_ pStateValue_ pStateReason_ =
  SetAlarmState'
    { alarmName = pAlarmName_,
      stateValue = pStateValue_,
      stateReason = pStateReason_,
      stateReasonData = Lude.Nothing
    }

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasAlarmName :: Lens.Lens' SetAlarmState Lude.Text
sasAlarmName = Lens.lens (alarmName :: SetAlarmState -> Lude.Text) (\s a -> s {alarmName = a} :: SetAlarmState)
{-# DEPRECATED sasAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The value of the state.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateValue :: Lens.Lens' SetAlarmState StateValue
sasStateValue = Lens.lens (stateValue :: SetAlarmState -> StateValue) (\s a -> s {stateValue = a} :: SetAlarmState)
{-# DEPRECATED sasStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The reason that this alarm is set to this specific state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateReason :: Lens.Lens' SetAlarmState Lude.Text
sasStateReason = Lens.lens (stateReason :: SetAlarmState -> Lude.Text) (\s a -> s {stateReason = a} :: SetAlarmState)
{-# DEPRECATED sasStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The reason that this alarm is set to this specific state, in JSON format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateReasonData :: Lens.Lens' SetAlarmState (Lude.Maybe Lude.Text)
sasStateReasonData = Lens.lens (stateReasonData :: SetAlarmState -> Lude.Maybe Lude.Text) (\s a -> s {stateReasonData = a} :: SetAlarmState)
{-# DEPRECATED sasStateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead." #-}

instance Lude.AWSRequest SetAlarmState where
  type Rs SetAlarmState = SetAlarmStateResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull SetAlarmStateResponse'

instance Lude.ToHeaders SetAlarmState where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetAlarmState where
  toPath = Lude.const "/"

instance Lude.ToQuery SetAlarmState where
  toQuery SetAlarmState' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetAlarmState" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmName" Lude.=: alarmName,
        "StateValue" Lude.=: stateValue,
        "StateReason" Lude.=: stateReason,
        "StateReasonData" Lude.=: stateReasonData
      ]

-- | /See:/ 'mkSetAlarmStateResponse' smart constructor.
data SetAlarmStateResponse = SetAlarmStateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetAlarmStateResponse' with the minimum fields required to make a request.
mkSetAlarmStateResponse ::
  SetAlarmStateResponse
mkSetAlarmStateResponse = SetAlarmStateResponse'
