{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyPushMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyPushMessage
  ( JourneyPushMessage (..),

    -- * Smart constructor
    mkJourneyPushMessage,

    -- * Lenses
    jpmTimeToLive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the message configuration for a push notification that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyPushMessage' smart constructor.
newtype JourneyPushMessage = JourneyPushMessage'
  { timeToLive ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyPushMessage' with the minimum fields required to make a request.
--
-- * 'timeToLive' - The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
mkJourneyPushMessage ::
  JourneyPushMessage
mkJourneyPushMessage =
  JourneyPushMessage' {timeToLive = Lude.Nothing}

-- | The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpmTimeToLive :: Lens.Lens' JourneyPushMessage (Lude.Maybe Lude.Text)
jpmTimeToLive = Lens.lens (timeToLive :: JourneyPushMessage -> Lude.Maybe Lude.Text) (\s a -> s {timeToLive = a} :: JourneyPushMessage)
{-# DEPRECATED jpmTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

instance Lude.FromJSON JourneyPushMessage where
  parseJSON =
    Lude.withObject
      "JourneyPushMessage"
      (\x -> JourneyPushMessage' Lude.<$> (x Lude..:? "TimeToLive"))

instance Lude.ToJSON JourneyPushMessage where
  toJSON JourneyPushMessage' {..} =
    Lude.object
      (Lude.catMaybes [("TimeToLive" Lude..=) Lude.<$> timeToLive])
