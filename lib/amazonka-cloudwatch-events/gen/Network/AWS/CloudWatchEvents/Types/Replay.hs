{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Replay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Replay where

import Network.AWS.CloudWatchEvents.Types.ReplayState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A @Replay@ object that contains details about a replay.
--
--
--
-- /See:/ 'replay' smart constructor.
data Replay = Replay'
  { _repEventSourceARN :: !(Maybe Text),
    _repState :: !(Maybe ReplayState),
    _repEventEndTime :: !(Maybe POSIX),
    _repReplayStartTime :: !(Maybe POSIX),
    _repReplayEndTime :: !(Maybe POSIX),
    _repEventLastReplayedTime :: !(Maybe POSIX),
    _repEventStartTime :: !(Maybe POSIX),
    _repReplayName :: !(Maybe Text),
    _repStateReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Replay' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'repEventSourceARN' - The ARN of the archive to replay event from.
--
-- * 'repState' - The current state of the replay.
--
-- * 'repEventEndTime' - A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
--
-- * 'repReplayStartTime' - A time stamp for the time that the replay started.
--
-- * 'repReplayEndTime' - A time stamp for the time that the replay completed.
--
-- * 'repEventLastReplayedTime' - A time stamp for the time that the last event was replayed.
--
-- * 'repEventStartTime' - A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
--
-- * 'repReplayName' - The name of the replay.
--
-- * 'repStateReason' - A description of why the replay is in the current state.
replay ::
  Replay
replay =
  Replay'
    { _repEventSourceARN = Nothing,
      _repState = Nothing,
      _repEventEndTime = Nothing,
      _repReplayStartTime = Nothing,
      _repReplayEndTime = Nothing,
      _repEventLastReplayedTime = Nothing,
      _repEventStartTime = Nothing,
      _repReplayName = Nothing,
      _repStateReason = Nothing
    }

-- | The ARN of the archive to replay event from.
repEventSourceARN :: Lens' Replay (Maybe Text)
repEventSourceARN = lens _repEventSourceARN (\s a -> s {_repEventSourceARN = a})

-- | The current state of the replay.
repState :: Lens' Replay (Maybe ReplayState)
repState = lens _repState (\s a -> s {_repState = a})

-- | A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
repEventEndTime :: Lens' Replay (Maybe UTCTime)
repEventEndTime = lens _repEventEndTime (\s a -> s {_repEventEndTime = a}) . mapping _Time

-- | A time stamp for the time that the replay started.
repReplayStartTime :: Lens' Replay (Maybe UTCTime)
repReplayStartTime = lens _repReplayStartTime (\s a -> s {_repReplayStartTime = a}) . mapping _Time

-- | A time stamp for the time that the replay completed.
repReplayEndTime :: Lens' Replay (Maybe UTCTime)
repReplayEndTime = lens _repReplayEndTime (\s a -> s {_repReplayEndTime = a}) . mapping _Time

-- | A time stamp for the time that the last event was replayed.
repEventLastReplayedTime :: Lens' Replay (Maybe UTCTime)
repEventLastReplayedTime = lens _repEventLastReplayedTime (\s a -> s {_repEventLastReplayedTime = a}) . mapping _Time

-- | A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
repEventStartTime :: Lens' Replay (Maybe UTCTime)
repEventStartTime = lens _repEventStartTime (\s a -> s {_repEventStartTime = a}) . mapping _Time

-- | The name of the replay.
repReplayName :: Lens' Replay (Maybe Text)
repReplayName = lens _repReplayName (\s a -> s {_repReplayName = a})

-- | A description of why the replay is in the current state.
repStateReason :: Lens' Replay (Maybe Text)
repStateReason = lens _repStateReason (\s a -> s {_repStateReason = a})

instance FromJSON Replay where
  parseJSON =
    withObject
      "Replay"
      ( \x ->
          Replay'
            <$> (x .:? "EventSourceArn")
            <*> (x .:? "State")
            <*> (x .:? "EventEndTime")
            <*> (x .:? "ReplayStartTime")
            <*> (x .:? "ReplayEndTime")
            <*> (x .:? "EventLastReplayedTime")
            <*> (x .:? "EventStartTime")
            <*> (x .:? "ReplayName")
            <*> (x .:? "StateReason")
      )

instance Hashable Replay

instance NFData Replay
