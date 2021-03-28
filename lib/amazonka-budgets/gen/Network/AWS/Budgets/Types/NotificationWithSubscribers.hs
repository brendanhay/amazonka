{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationWithSubscribers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.NotificationWithSubscribers
  ( NotificationWithSubscribers (..)
  -- * Smart constructor
  , mkNotificationWithSubscribers
  -- * Lenses
  , nwsNotification
  , nwsSubscribers
  ) where

import qualified Network.AWS.Budgets.Types.Notification as Types
import qualified Network.AWS.Budgets.Types.Subscriber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A notification with subscribers. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
-- /See:/ 'mkNotificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { notification :: Types.Notification
    -- ^ The notification that is associated with a budget.
  , subscribers :: Core.NonEmpty Types.Subscriber
    -- ^ A list of subscribers who are subscribed to this notification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationWithSubscribers' value with any optional fields omitted.
mkNotificationWithSubscribers
    :: Types.Notification -- ^ 'notification'
    -> Core.NonEmpty Types.Subscriber -- ^ 'subscribers'
    -> NotificationWithSubscribers
mkNotificationWithSubscribers notification subscribers
  = NotificationWithSubscribers'{notification, subscribers}

-- | The notification that is associated with a budget.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwsNotification :: Lens.Lens' NotificationWithSubscribers Types.Notification
nwsNotification = Lens.field @"notification"
{-# INLINEABLE nwsNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | A list of subscribers who are subscribed to this notification.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwsSubscribers :: Lens.Lens' NotificationWithSubscribers (Core.NonEmpty Types.Subscriber)
nwsSubscribers = Lens.field @"subscribers"
{-# INLINEABLE nwsSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

instance Core.FromJSON NotificationWithSubscribers where
        toJSON NotificationWithSubscribers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Notification" Core..= notification),
                  Core.Just ("Subscribers" Core..= subscribers)])
