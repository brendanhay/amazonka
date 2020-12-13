{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationWithSubscribers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.NotificationWithSubscribers
  ( NotificationWithSubscribers (..),

    -- * Smart constructor
    mkNotificationWithSubscribers,

    -- * Lenses
    nwsNotification,
    nwsSubscribers,
  )
where

import Network.AWS.Budgets.Types.Notification
import Network.AWS.Budgets.Types.Subscriber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A notification with subscribers. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
-- /See:/ 'mkNotificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { -- | The notification that is associated with a budget.
    notification :: Notification,
    -- | A list of subscribers who are subscribed to this notification.
    subscribers :: Lude.NonEmpty Subscriber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationWithSubscribers' with the minimum fields required to make a request.
--
-- * 'notification' - The notification that is associated with a budget.
-- * 'subscribers' - A list of subscribers who are subscribed to this notification.
mkNotificationWithSubscribers ::
  -- | 'notification'
  Notification ->
  -- | 'subscribers'
  Lude.NonEmpty Subscriber ->
  NotificationWithSubscribers
mkNotificationWithSubscribers pNotification_ pSubscribers_ =
  NotificationWithSubscribers'
    { notification = pNotification_,
      subscribers = pSubscribers_
    }

-- | The notification that is associated with a budget.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwsNotification :: Lens.Lens' NotificationWithSubscribers Notification
nwsNotification = Lens.lens (notification :: NotificationWithSubscribers -> Notification) (\s a -> s {notification = a} :: NotificationWithSubscribers)
{-# DEPRECATED nwsNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | A list of subscribers who are subscribed to this notification.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwsSubscribers :: Lens.Lens' NotificationWithSubscribers (Lude.NonEmpty Subscriber)
nwsSubscribers = Lens.lens (subscribers :: NotificationWithSubscribers -> Lude.NonEmpty Subscriber) (\s a -> s {subscribers = a} :: NotificationWithSubscribers)
{-# DEPRECATED nwsSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.ToJSON NotificationWithSubscribers where
  toJSON NotificationWithSubscribers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("Subscribers" Lude..= subscribers)
          ]
      )
