{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationWithSubscribers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.NotificationWithSubscribers where

import Network.AWS.Budgets.Types.Notification
import Network.AWS.Budgets.Types.Subscriber
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A notification with subscribers. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
--
--
-- /See:/ 'notificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { _nwsNotification ::
      !Notification,
    _nwsSubscribers ::
      !(List1 Subscriber)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationWithSubscribers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwsNotification' - The notification that is associated with a budget.
--
-- * 'nwsSubscribers' - A list of subscribers who are subscribed to this notification.
notificationWithSubscribers ::
  -- | 'nwsNotification'
  Notification ->
  -- | 'nwsSubscribers'
  NonEmpty Subscriber ->
  NotificationWithSubscribers
notificationWithSubscribers pNotification_ pSubscribers_ =
  NotificationWithSubscribers'
    { _nwsNotification = pNotification_,
      _nwsSubscribers = _List1 # pSubscribers_
    }

-- | The notification that is associated with a budget.
nwsNotification :: Lens' NotificationWithSubscribers Notification
nwsNotification = lens _nwsNotification (\s a -> s {_nwsNotification = a})

-- | A list of subscribers who are subscribed to this notification.
nwsSubscribers :: Lens' NotificationWithSubscribers (NonEmpty Subscriber)
nwsSubscribers = lens _nwsSubscribers (\s a -> s {_nwsSubscribers = a}) . _List1

instance Hashable NotificationWithSubscribers

instance NFData NotificationWithSubscribers

instance ToJSON NotificationWithSubscribers where
  toJSON NotificationWithSubscribers' {..} =
    object
      ( catMaybes
          [ Just ("Notification" .= _nwsNotification),
            Just ("Subscribers" .= _nwsSubscribers)
          ]
      )
