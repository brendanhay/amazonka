{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.SendTestEventNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendTestEventNotification@ operation causes Amazon Mechanical Turk to send a notification message as if a HIT event occurred, according to the provided notification specification. This allows you to test notifications without setting up notifications for a real HIT type and trying to trigger them using the website. When you call this operation, the service attempts to send the test notification immediately. 
module Network.AWS.MechanicalTurk.SendTestEventNotification
    (
    -- * Creating a request
      SendTestEventNotification (..)
    , mkSendTestEventNotification
    -- ** Request lenses
    , stenNotification
    , stenTestEventType

    -- * Destructuring the response
    , SendTestEventNotificationResponse (..)
    , mkSendTestEventNotificationResponse
    -- ** Response lenses
    , stenrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendTestEventNotification' smart constructor.
data SendTestEventNotification = SendTestEventNotification'
  { notification :: Types.NotificationSpecification
    -- ^ The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type. 
  , testEventType :: Types.EventType
    -- ^ The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendTestEventNotification' value with any optional fields omitted.
mkSendTestEventNotification
    :: Types.NotificationSpecification -- ^ 'notification'
    -> Types.EventType -- ^ 'testEventType'
    -> SendTestEventNotification
mkSendTestEventNotification notification testEventType
  = SendTestEventNotification'{notification, testEventType}

-- | The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type. 
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenNotification :: Lens.Lens' SendTestEventNotification Types.NotificationSpecification
stenNotification = Lens.field @"notification"
{-# INLINEABLE stenNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event. 
--
-- /Note:/ Consider using 'testEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenTestEventType :: Lens.Lens' SendTestEventNotification Types.EventType
stenTestEventType = Lens.field @"testEventType"
{-# INLINEABLE stenTestEventType #-}
{-# DEPRECATED testEventType "Use generic-lens or generic-optics with 'testEventType' instead"  #-}

instance Core.ToQuery SendTestEventNotification where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SendTestEventNotification where
        toHeaders SendTestEventNotification{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.SendTestEventNotification")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SendTestEventNotification where
        toJSON SendTestEventNotification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Notification" Core..= notification),
                  Core.Just ("TestEventType" Core..= testEventType)])

instance Core.AWSRequest SendTestEventNotification where
        type Rs SendTestEventNotification =
             SendTestEventNotificationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SendTestEventNotificationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendTestEventNotificationResponse' smart constructor.
newtype SendTestEventNotificationResponse = SendTestEventNotificationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendTestEventNotificationResponse' value with any optional fields omitted.
mkSendTestEventNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SendTestEventNotificationResponse
mkSendTestEventNotificationResponse responseStatus
  = SendTestEventNotificationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenrrsResponseStatus :: Lens.Lens' SendTestEventNotificationResponse Core.Int
stenrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stenrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
