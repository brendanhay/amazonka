{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.OutputLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.OutputLogEvent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a log event.
--
--
--
-- /See:/ 'outputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
  { _oleIngestionTime ::
      !(Maybe Nat),
    _oleMessage :: !(Maybe Text),
    _oleTimestamp :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oleIngestionTime' - The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'oleMessage' - The data contained in the log event.
--
-- * 'oleTimestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
outputLogEvent ::
  OutputLogEvent
outputLogEvent =
  OutputLogEvent'
    { _oleIngestionTime = Nothing,
      _oleMessage = Nothing,
      _oleTimestamp = Nothing
    }

-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
oleIngestionTime :: Lens' OutputLogEvent (Maybe Natural)
oleIngestionTime = lens _oleIngestionTime (\s a -> s {_oleIngestionTime = a}) . mapping _Nat

-- | The data contained in the log event.
oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\s a -> s {_oleMessage = a})

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\s a -> s {_oleTimestamp = a}) . mapping _Nat

instance FromJSON OutputLogEvent where
  parseJSON =
    withObject
      "OutputLogEvent"
      ( \x ->
          OutputLogEvent'
            <$> (x .:? "ingestionTime")
            <*> (x .:? "message")
            <*> (x .:? "timestamp")
      )

instance Hashable OutputLogEvent

instance NFData OutputLogEvent
