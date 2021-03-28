{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetAlarms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured alarms. Specify an alarm name in your request to return information about a specific alarm, or specify a monitored resource name to return information about all alarms for a specific resource.
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.GetAlarms
    (
    -- * Creating a request
      GetAlarms (..)
    , mkGetAlarms
    -- ** Request lenses
    , gaAlarmName
    , gaMonitoredResourceName
    , gaPageToken

    -- * Destructuring the response
    , GetAlarmsResponse (..)
    , mkGetAlarmsResponse
    -- ** Response lenses
    , garrsAlarms
    , garrsNextPageToken
    , garrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAlarms' smart constructor.
data GetAlarms = GetAlarms'
  { alarmName :: Core.Maybe Types.ResourceName
    -- ^ The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
  , monitoredResourceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms for a specific resource.
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAlarms' value with any optional fields omitted.
mkGetAlarms
    :: GetAlarms
mkGetAlarms
  = GetAlarms'{alarmName = Core.Nothing,
               monitoredResourceName = Core.Nothing, pageToken = Core.Nothing}

-- | The name of the alarm.
--
-- Specify an alarm name to return information about a specific alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAlarmName :: Lens.Lens' GetAlarms (Core.Maybe Types.ResourceName)
gaAlarmName = Lens.field @"alarmName"
{-# INLINEABLE gaAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The name of the Lightsail resource being monitored by the alarm.
--
-- Specify a monitored resource name to return information about all alarms for a specific resource.
--
-- /Note:/ Consider using 'monitoredResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMonitoredResourceName :: Lens.Lens' GetAlarms (Core.Maybe Types.ResourceName)
gaMonitoredResourceName = Lens.field @"monitoredResourceName"
{-# INLINEABLE gaMonitoredResourceName #-}
{-# DEPRECATED monitoredResourceName "Use generic-lens or generic-optics with 'monitoredResourceName' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetAlarms@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPageToken :: Lens.Lens' GetAlarms (Core.Maybe Core.Text)
gaPageToken = Lens.field @"pageToken"
{-# INLINEABLE gaPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetAlarms where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAlarms where
        toHeaders GetAlarms{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetAlarms")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAlarms where
        toJSON GetAlarms{..}
          = Core.object
              (Core.catMaybes
                 [("alarmName" Core..=) Core.<$> alarmName,
                  ("monitoredResourceName" Core..=) Core.<$> monitoredResourceName,
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetAlarms where
        type Rs GetAlarms = GetAlarmsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAlarmsResponse' Core.<$>
                   (x Core..:? "alarms") Core.<*> x Core..:? "nextPageToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAlarmsResponse' smart constructor.
data GetAlarmsResponse = GetAlarmsResponse'
  { alarms :: Core.Maybe [Types.Alarm]
    -- ^ An array of objects that describe the alarms.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAlarmsResponse' value with any optional fields omitted.
mkGetAlarmsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAlarmsResponse
mkGetAlarmsResponse responseStatus
  = GetAlarmsResponse'{alarms = Core.Nothing,
                       nextPageToken = Core.Nothing, responseStatus}

-- | An array of objects that describe the alarms.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAlarms :: Lens.Lens' GetAlarmsResponse (Core.Maybe [Types.Alarm])
garrsAlarms = Lens.field @"alarms"
{-# INLINEABLE garrsAlarms #-}
{-# DEPRECATED alarms "Use generic-lens or generic-optics with 'alarms' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetAlarms@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsNextPageToken :: Lens.Lens' GetAlarmsResponse (Core.Maybe Core.Text)
garrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE garrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAlarmsResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
