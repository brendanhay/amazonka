{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyPushMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyPushMessage
  ( JourneyPushMessage (..)
  -- * Smart constructor
  , mkJourneyPushMessage
  -- * Lenses
  , jpmTimeToLive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the message configuration for a push notification that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyPushMessage' smart constructor.
newtype JourneyPushMessage = JourneyPushMessage'
  { timeToLive :: Core.Maybe Core.Text
    -- ^ The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyPushMessage' value with any optional fields omitted.
mkJourneyPushMessage
    :: JourneyPushMessage
mkJourneyPushMessage
  = JourneyPushMessage'{timeToLive = Core.Nothing}

-- | The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpmTimeToLive :: Lens.Lens' JourneyPushMessage (Core.Maybe Core.Text)
jpmTimeToLive = Lens.field @"timeToLive"
{-# INLINEABLE jpmTimeToLive #-}
{-# DEPRECATED timeToLive "Use generic-lens or generic-optics with 'timeToLive' instead"  #-}

instance Core.FromJSON JourneyPushMessage where
        toJSON JourneyPushMessage{..}
          = Core.object
              (Core.catMaybes [("TimeToLive" Core..=) Core.<$> timeToLive])

instance Core.FromJSON JourneyPushMessage where
        parseJSON
          = Core.withObject "JourneyPushMessage" Core.$
              \ x -> JourneyPushMessage' Core.<$> (x Core..:? "TimeToLive")
