{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteAlarm (..)
    , mkDeleteAlarm
    -- ** Request lenses
    , daAlarmName

    -- * Destructuring the response
    , DeleteAlarmResponse (..)
    , mkDeleteAlarmResponse
    -- ** Response lenses
    , darrsOperations
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAlarm' smart constructor.
newtype DeleteAlarm = DeleteAlarm'
  { alarmName :: Types.ResourceName
    -- ^ The name of the alarm to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlarm' value with any optional fields omitted.
mkDeleteAlarm
    :: Types.ResourceName -- ^ 'alarmName'
    -> DeleteAlarm
mkDeleteAlarm alarmName = DeleteAlarm'{alarmName}

-- | The name of the alarm to delete.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmName :: Lens.Lens' DeleteAlarm Types.ResourceName
daAlarmName = Lens.field @"alarmName"
{-# INLINEABLE daAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

instance Core.ToQuery DeleteAlarm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAlarm where
        toHeaders DeleteAlarm{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteAlarm")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAlarm where
        toJSON DeleteAlarm{..}
          = Core.object
              (Core.catMaybes [Core.Just ("alarmName" Core..= alarmName)])

instance Core.AWSRequest DeleteAlarm where
        type Rs DeleteAlarm = DeleteAlarmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteAlarmResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteAlarmResponse' value with any optional fields omitted.
mkDeleteAlarmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAlarmResponse
mkDeleteAlarmResponse responseStatus
  = DeleteAlarmResponse'{operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsOperations :: Lens.Lens' DeleteAlarmResponse (Core.Maybe [Types.Operation])
darrsOperations = Lens.field @"operations"
{-# INLINEABLE darrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAlarmResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
