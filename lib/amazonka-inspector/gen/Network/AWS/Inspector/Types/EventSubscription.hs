{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.EventSubscription where

import Network.AWS.Inspector.Types.InspectorEvent
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used in the 'Subscription' data type.
--
--
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { _esEvent ::
      !InspectorEvent,
    _esSubscribedAt :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esEvent' - The event for which Amazon Simple Notification Service (SNS) notifications are sent.
--
-- * 'esSubscribedAt' - The time at which 'SubscribeToEvent' is called.
eventSubscription ::
  -- | 'esEvent'
  InspectorEvent ->
  -- | 'esSubscribedAt'
  UTCTime ->
  EventSubscription
eventSubscription pEvent_ pSubscribedAt_ =
  EventSubscription'
    { _esEvent = pEvent_,
      _esSubscribedAt = _Time # pSubscribedAt_
    }

-- | The event for which Amazon Simple Notification Service (SNS) notifications are sent.
esEvent :: Lens' EventSubscription InspectorEvent
esEvent = lens _esEvent (\s a -> s {_esEvent = a})

-- | The time at which 'SubscribeToEvent' is called.
esSubscribedAt :: Lens' EventSubscription UTCTime
esSubscribedAt = lens _esSubscribedAt (\s a -> s {_esSubscribedAt = a}) . _Time

instance FromJSON EventSubscription where
  parseJSON =
    withObject
      "EventSubscription"
      ( \x ->
          EventSubscription' <$> (x .: "event") <*> (x .: "subscribedAt")
      )

instance Hashable EventSubscription

instance NFData EventSubscription
