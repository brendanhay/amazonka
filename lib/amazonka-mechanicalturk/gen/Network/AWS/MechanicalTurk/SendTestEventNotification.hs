{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SendTestEventNotification (..),
    mkSendTestEventNotification,

    -- ** Request lenses
    stenTestEventType,
    stenNotification,

    -- * Destructuring the response
    SendTestEventNotificationResponse (..),
    mkSendTestEventNotificationResponse,

    -- ** Response lenses
    stenrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendTestEventNotification' smart constructor.
data SendTestEventNotification = SendTestEventNotification'
  { -- | The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event.
    testEventType :: EventType,
    -- | The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type.
    notification :: NotificationSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendTestEventNotification' with the minimum fields required to make a request.
--
-- * 'testEventType' - The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event.
-- * 'notification' - The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type.
mkSendTestEventNotification ::
  -- | 'testEventType'
  EventType ->
  -- | 'notification'
  NotificationSpecification ->
  SendTestEventNotification
mkSendTestEventNotification pTestEventType_ pNotification_ =
  SendTestEventNotification'
    { testEventType = pTestEventType_,
      notification = pNotification_
    }

-- | The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event.
--
-- /Note:/ Consider using 'testEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenTestEventType :: Lens.Lens' SendTestEventNotification EventType
stenTestEventType = Lens.lens (testEventType :: SendTestEventNotification -> EventType) (\s a -> s {testEventType = a} :: SendTestEventNotification)
{-# DEPRECATED stenTestEventType "Use generic-lens or generic-optics with 'testEventType' instead." #-}

-- | The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenNotification :: Lens.Lens' SendTestEventNotification NotificationSpecification
stenNotification = Lens.lens (notification :: SendTestEventNotification -> NotificationSpecification) (\s a -> s {notification = a} :: SendTestEventNotification)
{-# DEPRECATED stenNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

instance Lude.AWSRequest SendTestEventNotification where
  type
    Rs SendTestEventNotification =
      SendTestEventNotificationResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SendTestEventNotificationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendTestEventNotification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.SendTestEventNotification" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendTestEventNotification where
  toJSON SendTestEventNotification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TestEventType" Lude..= testEventType),
            Lude.Just ("Notification" Lude..= notification)
          ]
      )

instance Lude.ToPath SendTestEventNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery SendTestEventNotification where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendTestEventNotificationResponse' smart constructor.
newtype SendTestEventNotificationResponse = SendTestEventNotificationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendTestEventNotificationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSendTestEventNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendTestEventNotificationResponse
mkSendTestEventNotificationResponse pResponseStatus_ =
  SendTestEventNotificationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stenrsResponseStatus :: Lens.Lens' SendTestEventNotificationResponse Lude.Int
stenrsResponseStatus = Lens.lens (responseStatus :: SendTestEventNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendTestEventNotificationResponse)
{-# DEPRECATED stenrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
