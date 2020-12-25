{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alarm.
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.DeleteAlarm
  ( -- * Creating a request
    DeleteAlarm (..),
    mkDeleteAlarm,

    -- ** Request lenses
    daAlarmName,

    -- * Destructuring the response
    DeleteAlarmResponse (..),
    mkDeleteAlarmResponse,

    -- ** Response lenses
    darrsOperations,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAlarm' smart constructor.
newtype DeleteAlarm = DeleteAlarm'
  { -- | The name of the alarm to delete.
    alarmName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlarm' value with any optional fields omitted.
mkDeleteAlarm ::
  -- | 'alarmName'
  Types.ResourceName ->
  DeleteAlarm
mkDeleteAlarm alarmName = DeleteAlarm' {alarmName}

-- | The name of the alarm to delete.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmName :: Lens.Lens' DeleteAlarm Types.ResourceName
daAlarmName = Lens.field @"alarmName"
{-# DEPRECATED daAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

instance Core.FromJSON DeleteAlarm where
  toJSON DeleteAlarm {..} =
    Core.object
      (Core.catMaybes [Core.Just ("alarmName" Core..= alarmName)])

instance Core.AWSRequest DeleteAlarm where
  type Rs DeleteAlarm = DeleteAlarmResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteAlarm")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAlarmResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteAlarmResponse' value with any optional fields omitted.
mkDeleteAlarmResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAlarmResponse
mkDeleteAlarmResponse responseStatus =
  DeleteAlarmResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsOperations :: Lens.Lens' DeleteAlarmResponse (Core.Maybe [Types.Operation])
darrsOperations = Lens.field @"operations"
{-# DEPRECATED darrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAlarmResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
