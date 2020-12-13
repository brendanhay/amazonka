{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.TestAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests an alarm by displaying a banner on the Amazon Lightsail console. If a notification trigger is configured for the specified alarm, the test also sends a notification to the notification protocol (@Email@ and/or @SMS@ ) configured for the alarm.
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.TestAlarm
  ( -- * Creating a request
    TestAlarm (..),
    mkTestAlarm,

    -- ** Request lenses
    taAlarmName,
    taState,

    -- * Destructuring the response
    TestAlarmResponse (..),
    mkTestAlarmResponse,

    -- ** Response lenses
    tarsOperations,
    tarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestAlarm' smart constructor.
data TestAlarm = TestAlarm'
  { -- | The name of the alarm to test.
    alarmName :: Lude.Text,
    -- | The alarm state to test.
    --
    -- An alarm has the following possible states that can be tested:
    --
    --     * @ALARM@ - The metric is outside of the defined threshold.
    --
    --
    --     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
    --
    --
    --     * @OK@ - The metric is within the defined threshold.
    state :: AlarmState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestAlarm' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name of the alarm to test.
-- * 'state' - The alarm state to test.
--
-- An alarm has the following possible states that can be tested:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
mkTestAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'state'
  AlarmState ->
  TestAlarm
mkTestAlarm pAlarmName_ pState_ =
  TestAlarm' {alarmName = pAlarmName_, state = pState_}

-- | The name of the alarm to test.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAlarmName :: Lens.Lens' TestAlarm Lude.Text
taAlarmName = Lens.lens (alarmName :: TestAlarm -> Lude.Text) (\s a -> s {alarmName = a} :: TestAlarm)
{-# DEPRECATED taAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The alarm state to test.
--
-- An alarm has the following possible states that can be tested:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taState :: Lens.Lens' TestAlarm AlarmState
taState = Lens.lens (state :: TestAlarm -> AlarmState) (\s a -> s {state = a} :: TestAlarm)
{-# DEPRECATED taState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.AWSRequest TestAlarm where
  type Rs TestAlarm = TestAlarmResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestAlarmResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestAlarm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.TestAlarm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TestAlarm where
  toJSON TestAlarm' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("alarmName" Lude..= alarmName),
            Lude.Just ("state" Lude..= state)
          ]
      )

instance Lude.ToPath TestAlarm where
  toPath = Lude.const "/"

instance Lude.ToQuery TestAlarm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTestAlarmResponse' smart constructor.
data TestAlarmResponse = TestAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestAlarmResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkTestAlarmResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestAlarmResponse
mkTestAlarmResponse pResponseStatus_ =
  TestAlarmResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsOperations :: Lens.Lens' TestAlarmResponse (Lude.Maybe [Operation])
tarsOperations = Lens.lens (operations :: TestAlarmResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: TestAlarmResponse)
{-# DEPRECATED tarsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResponseStatus :: Lens.Lens' TestAlarmResponse Lude.Int
tarsResponseStatus = Lens.lens (responseStatus :: TestAlarmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestAlarmResponse)
{-# DEPRECATED tarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
