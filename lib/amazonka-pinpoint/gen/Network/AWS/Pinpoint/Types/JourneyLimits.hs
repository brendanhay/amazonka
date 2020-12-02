{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies limits on the messages that a journey can send and the number of times participants can enter a journey.
--
--
--
-- /See:/ 'journeyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { _jlMessagesPerSecond ::
      !(Maybe Int),
    _jlEndpointReentryCap :: !(Maybe Int),
    _jlDailyCap :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jlMessagesPerSecond' - The maximum number of messages that the journey can send each second.
--
-- * 'jlEndpointReentryCap' - The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
--
-- * 'jlDailyCap' - The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
journeyLimits ::
  JourneyLimits
journeyLimits =
  JourneyLimits'
    { _jlMessagesPerSecond = Nothing,
      _jlEndpointReentryCap = Nothing,
      _jlDailyCap = Nothing
    }

-- | The maximum number of messages that the journey can send each second.
jlMessagesPerSecond :: Lens' JourneyLimits (Maybe Int)
jlMessagesPerSecond = lens _jlMessagesPerSecond (\s a -> s {_jlMessagesPerSecond = a})

-- | The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
jlEndpointReentryCap :: Lens' JourneyLimits (Maybe Int)
jlEndpointReentryCap = lens _jlEndpointReentryCap (\s a -> s {_jlEndpointReentryCap = a})

-- | The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
jlDailyCap :: Lens' JourneyLimits (Maybe Int)
jlDailyCap = lens _jlDailyCap (\s a -> s {_jlDailyCap = a})

instance FromJSON JourneyLimits where
  parseJSON =
    withObject
      "JourneyLimits"
      ( \x ->
          JourneyLimits'
            <$> (x .:? "MessagesPerSecond")
            <*> (x .:? "EndpointReentryCap")
            <*> (x .:? "DailyCap")
      )

instance Hashable JourneyLimits

instance NFData JourneyLimits

instance ToJSON JourneyLimits where
  toJSON JourneyLimits' {..} =
    object
      ( catMaybes
          [ ("MessagesPerSecond" .=) <$> _jlMessagesPerSecond,
            ("EndpointReentryCap" .=) <$> _jlEndpointReentryCap,
            ("DailyCap" .=) <$> _jlDailyCap
          ]
      )
