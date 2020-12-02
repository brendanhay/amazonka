{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.InputLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.InputLogEvent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.
--
--
--
-- /See:/ 'inputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { _ileTimestamp :: !Nat,
    _ileMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ileTimestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'ileMessage' - The raw event message.
inputLogEvent ::
  -- | 'ileTimestamp'
  Natural ->
  -- | 'ileMessage'
  Text ->
  InputLogEvent
inputLogEvent pTimestamp_ pMessage_ =
  InputLogEvent'
    { _ileTimestamp = _Nat # pTimestamp_,
      _ileMessage = pMessage_
    }

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\s a -> s {_ileTimestamp = a}) . _Nat

-- | The raw event message.
ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\s a -> s {_ileMessage = a})

instance Hashable InputLogEvent

instance NFData InputLogEvent

instance ToJSON InputLogEvent where
  toJSON InputLogEvent' {..} =
    object
      ( catMaybes
          [ Just ("timestamp" .= _ileTimestamp),
            Just ("message" .= _ileMessage)
          ]
      )
