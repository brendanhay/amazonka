{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Replay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Replay
  ( Replay (..),

    -- * Smart constructor
    mkReplay,

    -- * Lenses
    rfEventSourceARN,
    rfState,
    rfEventEndTime,
    rfReplayStartTime,
    rfReplayEndTime,
    rfEventLastReplayedTime,
    rfEventStartTime,
    rfReplayName,
    rfStateReason,
  )
where

import Network.AWS.CloudWatchEvents.Types.ReplayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @Replay@ object that contains details about a replay.
--
-- /See:/ 'mkReplay' smart constructor.
data Replay = Replay'
  { -- | The ARN of the archive to replay event from.
    eventSourceARN :: Lude.Maybe Lude.Text,
    -- | The current state of the replay.
    state :: Lude.Maybe ReplayState,
    -- | A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
    eventEndTime :: Lude.Maybe Lude.Timestamp,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Lude.Maybe Lude.Timestamp,
    -- | A time stamp for the time that the replay completed.
    replayEndTime :: Lude.Maybe Lude.Timestamp,
    -- | A time stamp for the time that the last event was replayed.
    eventLastReplayedTime :: Lude.Maybe Lude.Timestamp,
    -- | A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
    eventStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the replay.
    replayName :: Lude.Maybe Lude.Text,
    -- | A description of why the replay is in the current state.
    stateReason :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Replay' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the archive to replay event from.
-- * 'state' - The current state of the replay.
-- * 'eventEndTime' - A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
-- * 'replayStartTime' - A time stamp for the time that the replay started.
-- * 'replayEndTime' - A time stamp for the time that the replay completed.
-- * 'eventLastReplayedTime' - A time stamp for the time that the last event was replayed.
-- * 'eventStartTime' - A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
-- * 'replayName' - The name of the replay.
-- * 'stateReason' - A description of why the replay is in the current state.
mkReplay ::
  Replay
mkReplay =
  Replay'
    { eventSourceARN = Lude.Nothing,
      state = Lude.Nothing,
      eventEndTime = Lude.Nothing,
      replayStartTime = Lude.Nothing,
      replayEndTime = Lude.Nothing,
      eventLastReplayedTime = Lude.Nothing,
      eventStartTime = Lude.Nothing,
      replayName = Lude.Nothing,
      stateReason = Lude.Nothing
    }

-- | The ARN of the archive to replay event from.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventSourceARN :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
rfEventSourceARN = Lens.lens (eventSourceARN :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: Replay)
{-# DEPRECATED rfEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfState :: Lens.Lens' Replay (Lude.Maybe ReplayState)
rfState = Lens.lens (state :: Replay -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: Replay)
{-# DEPRECATED rfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventEndTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
rfEventEndTime = Lens.lens (eventEndTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventEndTime = a} :: Replay)
{-# DEPRECATED rfEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayStartTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
rfReplayStartTime = Lens.lens (replayStartTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayStartTime = a} :: Replay)
{-# DEPRECATED rfReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | A time stamp for the time that the replay completed.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayEndTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
rfReplayEndTime = Lens.lens (replayEndTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayEndTime = a} :: Replay)
{-# DEPRECATED rfReplayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead." #-}

-- | A time stamp for the time that the last event was replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventLastReplayedTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
rfEventLastReplayedTime = Lens.lens (eventLastReplayedTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventLastReplayedTime = a} :: Replay)
{-# DEPRECATED rfEventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead." #-}

-- | A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventStartTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
rfEventStartTime = Lens.lens (eventStartTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventStartTime = a} :: Replay)
{-# DEPRECATED rfEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayName :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
rfReplayName = Lens.lens (replayName :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {replayName = a} :: Replay)
{-# DEPRECATED rfReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | A description of why the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfStateReason :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
rfStateReason = Lens.lens (stateReason :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: Replay)
{-# DEPRECATED rfStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

instance Lude.FromJSON Replay where
  parseJSON =
    Lude.withObject
      "Replay"
      ( \x ->
          Replay'
            Lude.<$> (x Lude..:? "EventSourceArn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "EventEndTime")
            Lude.<*> (x Lude..:? "ReplayStartTime")
            Lude.<*> (x Lude..:? "ReplayEndTime")
            Lude.<*> (x Lude..:? "EventLastReplayedTime")
            Lude.<*> (x Lude..:? "EventStartTime")
            Lude.<*> (x Lude..:? "ReplayName")
            Lude.<*> (x Lude..:? "StateReason")
      )
