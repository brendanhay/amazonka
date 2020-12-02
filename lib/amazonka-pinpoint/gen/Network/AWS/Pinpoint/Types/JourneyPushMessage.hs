{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyPushMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyPushMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the message configuration for a push notification that's sent to participants in a journey.
--
--
--
-- /See:/ 'journeyPushMessage' smart constructor.
newtype JourneyPushMessage = JourneyPushMessage'
  { _jpmTimeToLive ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyPushMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jpmTimeToLive' - The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again. This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
journeyPushMessage ::
  JourneyPushMessage
journeyPushMessage = JourneyPushMessage' {_jpmTimeToLive = Nothing}

-- | The number of seconds that the push notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again. This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
jpmTimeToLive :: Lens' JourneyPushMessage (Maybe Text)
jpmTimeToLive = lens _jpmTimeToLive (\s a -> s {_jpmTimeToLive = a})

instance FromJSON JourneyPushMessage where
  parseJSON =
    withObject
      "JourneyPushMessage"
      (\x -> JourneyPushMessage' <$> (x .:? "TimeToLive"))

instance Hashable JourneyPushMessage

instance NFData JourneyPushMessage

instance ToJSON JourneyPushMessage where
  toJSON JourneyPushMessage' {..} =
    object (catMaybes [("TimeToLive" .=) <$> _jpmTimeToLive])
