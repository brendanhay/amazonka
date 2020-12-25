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
    tarrsOperations,
    tarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestAlarm' smart constructor.
data TestAlarm = TestAlarm'
  { -- | The name of the alarm to test.
    alarmName :: Types.AlarmName,
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
    state :: Types.AlarmState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestAlarm' value with any optional fields omitted.
mkTestAlarm ::
  -- | 'alarmName'
  Types.AlarmName ->
  -- | 'state'
  Types.AlarmState ->
  TestAlarm
mkTestAlarm alarmName state = TestAlarm' {alarmName, state}

-- | The name of the alarm to test.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAlarmName :: Lens.Lens' TestAlarm Types.AlarmName
taAlarmName = Lens.field @"alarmName"
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
taState :: Lens.Lens' TestAlarm Types.AlarmState
taState = Lens.field @"state"
{-# DEPRECATED taState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON TestAlarm where
  toJSON TestAlarm {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("alarmName" Core..= alarmName),
            Core.Just ("state" Core..= state)
          ]
      )

instance Core.AWSRequest TestAlarm where
  type Rs TestAlarm = TestAlarmResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.TestAlarm")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TestAlarmResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestAlarmResponse' smart constructor.
data TestAlarmResponse = TestAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TestAlarmResponse' value with any optional fields omitted.
mkTestAlarmResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestAlarmResponse
mkTestAlarmResponse responseStatus =
  TestAlarmResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarrsOperations :: Lens.Lens' TestAlarmResponse (Core.Maybe [Types.Operation])
tarrsOperations = Lens.field @"operations"
{-# DEPRECATED tarrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarrsResponseStatus :: Lens.Lens' TestAlarmResponse Core.Int
tarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
