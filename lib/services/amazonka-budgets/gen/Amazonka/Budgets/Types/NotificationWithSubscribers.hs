{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Budgets.Types.NotificationWithSubscribers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.NotificationWithSubscribers where

import Amazonka.Budgets.Types.Notification
import Amazonka.Budgets.Types.Subscriber
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A notification with subscribers. A notification can have one SNS
-- subscriber and up to 10 email subscribers, for a total of 11
-- subscribers.
--
-- /See:/ 'newNotificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { -- | The notification that\'s associated with a budget.
    notification :: Notification,
    -- | A list of subscribers who are subscribed to this notification.
    subscribers :: Prelude.NonEmpty Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationWithSubscribers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notification', 'notificationWithSubscribers_notification' - The notification that\'s associated with a budget.
--
-- 'subscribers', 'notificationWithSubscribers_subscribers' - A list of subscribers who are subscribed to this notification.
newNotificationWithSubscribers ::
  -- | 'notification'
  Notification ->
  -- | 'subscribers'
  Prelude.NonEmpty Subscriber ->
  NotificationWithSubscribers
newNotificationWithSubscribers
  pNotification_
  pSubscribers_ =
    NotificationWithSubscribers'
      { notification =
          pNotification_,
        subscribers =
          Lens.coerced Lens.# pSubscribers_
      }

-- | The notification that\'s associated with a budget.
notificationWithSubscribers_notification :: Lens.Lens' NotificationWithSubscribers Notification
notificationWithSubscribers_notification = Lens.lens (\NotificationWithSubscribers' {notification} -> notification) (\s@NotificationWithSubscribers' {} a -> s {notification = a} :: NotificationWithSubscribers)

-- | A list of subscribers who are subscribed to this notification.
notificationWithSubscribers_subscribers :: Lens.Lens' NotificationWithSubscribers (Prelude.NonEmpty Subscriber)
notificationWithSubscribers_subscribers = Lens.lens (\NotificationWithSubscribers' {subscribers} -> subscribers) (\s@NotificationWithSubscribers' {} a -> s {subscribers = a} :: NotificationWithSubscribers) Prelude.. Lens.coerced

instance Prelude.Hashable NotificationWithSubscribers where
  hashWithSalt _salt NotificationWithSubscribers' {..} =
    _salt
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` subscribers

instance Prelude.NFData NotificationWithSubscribers where
  rnf NotificationWithSubscribers' {..} =
    Prelude.rnf notification `Prelude.seq`
      Prelude.rnf subscribers

instance Data.ToJSON NotificationWithSubscribers where
  toJSON NotificationWithSubscribers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Notification" Data..= notification),
            Prelude.Just ("Subscribers" Data..= subscribers)
          ]
      )
