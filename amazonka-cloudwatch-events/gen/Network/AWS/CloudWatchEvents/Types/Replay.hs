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
-- Module      : Network.AWS.CloudWatchEvents.Types.Replay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Replay where

import Network.AWS.CloudWatchEvents.Types.ReplayState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A @Replay@ object that contains details about a replay.
--
-- /See:/ 'newReplay' smart constructor.
data Replay = Replay'
  { -- | The ARN of the archive to replay event from.
    eventSourceArn :: Core.Maybe Core.Text,
    -- | A time stamp for the time to start replaying events. This is determined
    -- by the time in the event as described in
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
    eventStartTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Core.Maybe Core.POSIX,
    -- | A description of why the replay is in the current state.
    stateReason :: Core.Maybe Core.Text,
    -- | The current state of the replay.
    state :: Core.Maybe ReplayState,
    -- | The name of the replay.
    replayName :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the last event was replayed.
    eventLastReplayedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay completed.
    replayEndTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time to start replaying events. Any event with a
    -- creation time prior to the @EventEndTime@ specified is replayed.
    eventEndTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Replay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'replay_eventSourceArn' - The ARN of the archive to replay event from.
--
-- 'eventStartTime', 'replay_eventStartTime' - A time stamp for the time to start replaying events. This is determined
-- by the time in the event as described in
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
--
-- 'replayStartTime', 'replay_replayStartTime' - A time stamp for the time that the replay started.
--
-- 'stateReason', 'replay_stateReason' - A description of why the replay is in the current state.
--
-- 'state', 'replay_state' - The current state of the replay.
--
-- 'replayName', 'replay_replayName' - The name of the replay.
--
-- 'eventLastReplayedTime', 'replay_eventLastReplayedTime' - A time stamp for the time that the last event was replayed.
--
-- 'replayEndTime', 'replay_replayEndTime' - A time stamp for the time that the replay completed.
--
-- 'eventEndTime', 'replay_eventEndTime' - A time stamp for the time to start replaying events. Any event with a
-- creation time prior to the @EventEndTime@ specified is replayed.
newReplay ::
  Replay
newReplay =
  Replay'
    { eventSourceArn = Core.Nothing,
      eventStartTime = Core.Nothing,
      replayStartTime = Core.Nothing,
      stateReason = Core.Nothing,
      state = Core.Nothing,
      replayName = Core.Nothing,
      eventLastReplayedTime = Core.Nothing,
      replayEndTime = Core.Nothing,
      eventEndTime = Core.Nothing
    }

-- | The ARN of the archive to replay event from.
replay_eventSourceArn :: Lens.Lens' Replay (Core.Maybe Core.Text)
replay_eventSourceArn = Lens.lens (\Replay' {eventSourceArn} -> eventSourceArn) (\s@Replay' {} a -> s {eventSourceArn = a} :: Replay)

-- | A time stamp for the time to start replaying events. This is determined
-- by the time in the event as described in
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
replay_eventStartTime :: Lens.Lens' Replay (Core.Maybe Core.UTCTime)
replay_eventStartTime = Lens.lens (\Replay' {eventStartTime} -> eventStartTime) (\s@Replay' {} a -> s {eventStartTime = a} :: Replay) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay started.
replay_replayStartTime :: Lens.Lens' Replay (Core.Maybe Core.UTCTime)
replay_replayStartTime = Lens.lens (\Replay' {replayStartTime} -> replayStartTime) (\s@Replay' {} a -> s {replayStartTime = a} :: Replay) Core.. Lens.mapping Core._Time

-- | A description of why the replay is in the current state.
replay_stateReason :: Lens.Lens' Replay (Core.Maybe Core.Text)
replay_stateReason = Lens.lens (\Replay' {stateReason} -> stateReason) (\s@Replay' {} a -> s {stateReason = a} :: Replay)

-- | The current state of the replay.
replay_state :: Lens.Lens' Replay (Core.Maybe ReplayState)
replay_state = Lens.lens (\Replay' {state} -> state) (\s@Replay' {} a -> s {state = a} :: Replay)

-- | The name of the replay.
replay_replayName :: Lens.Lens' Replay (Core.Maybe Core.Text)
replay_replayName = Lens.lens (\Replay' {replayName} -> replayName) (\s@Replay' {} a -> s {replayName = a} :: Replay)

-- | A time stamp for the time that the last event was replayed.
replay_eventLastReplayedTime :: Lens.Lens' Replay (Core.Maybe Core.UTCTime)
replay_eventLastReplayedTime = Lens.lens (\Replay' {eventLastReplayedTime} -> eventLastReplayedTime) (\s@Replay' {} a -> s {eventLastReplayedTime = a} :: Replay) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay completed.
replay_replayEndTime :: Lens.Lens' Replay (Core.Maybe Core.UTCTime)
replay_replayEndTime = Lens.lens (\Replay' {replayEndTime} -> replayEndTime) (\s@Replay' {} a -> s {replayEndTime = a} :: Replay) Core.. Lens.mapping Core._Time

-- | A time stamp for the time to start replaying events. Any event with a
-- creation time prior to the @EventEndTime@ specified is replayed.
replay_eventEndTime :: Lens.Lens' Replay (Core.Maybe Core.UTCTime)
replay_eventEndTime = Lens.lens (\Replay' {eventEndTime} -> eventEndTime) (\s@Replay' {} a -> s {eventEndTime = a} :: Replay) Core.. Lens.mapping Core._Time

instance Core.FromJSON Replay where
  parseJSON =
    Core.withObject
      "Replay"
      ( \x ->
          Replay'
            Core.<$> (x Core..:? "EventSourceArn")
            Core.<*> (x Core..:? "EventStartTime")
            Core.<*> (x Core..:? "ReplayStartTime")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "ReplayName")
            Core.<*> (x Core..:? "EventLastReplayedTime")
            Core.<*> (x Core..:? "ReplayEndTime")
            Core.<*> (x Core..:? "EventEndTime")
      )

instance Core.Hashable Replay

instance Core.NFData Replay
