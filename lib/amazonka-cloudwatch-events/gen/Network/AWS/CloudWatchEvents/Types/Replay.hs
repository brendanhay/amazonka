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
    repEventSourceARN,
    repState,
    repEventEndTime,
    repReplayStartTime,
    repReplayEndTime,
    repEventLastReplayedTime,
    repEventStartTime,
    repReplayName,
    repStateReason,
  )
where

import Network.AWS.CloudWatchEvents.Types.ReplayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @Replay@ object that contains details about a replay.
--
-- /See:/ 'mkReplay' smart constructor.
data Replay = Replay'
  { eventSourceARN :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe ReplayState,
    eventEndTime :: Lude.Maybe Lude.Timestamp,
    replayStartTime :: Lude.Maybe Lude.Timestamp,
    replayEndTime :: Lude.Maybe Lude.Timestamp,
    eventLastReplayedTime :: Lude.Maybe Lude.Timestamp,
    eventStartTime :: Lude.Maybe Lude.Timestamp,
    replayName :: Lude.Maybe Lude.Text,
    stateReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Replay' with the minimum fields required to make a request.
--
-- * 'eventEndTime' - A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
-- * 'eventLastReplayedTime' - A time stamp for the time that the last event was replayed.
-- * 'eventSourceARN' - The ARN of the archive to replay event from.
-- * 'eventStartTime' - A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
-- * 'replayEndTime' - A time stamp for the time that the replay completed.
-- * 'replayName' - The name of the replay.
-- * 'replayStartTime' - A time stamp for the time that the replay started.
-- * 'state' - The current state of the replay.
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
repEventSourceARN :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
repEventSourceARN = Lens.lens (eventSourceARN :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: Replay)
{-# DEPRECATED repEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repState :: Lens.Lens' Replay (Lude.Maybe ReplayState)
repState = Lens.lens (state :: Replay -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: Replay)
{-# DEPRECATED repState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repEventEndTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
repEventEndTime = Lens.lens (eventEndTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventEndTime = a} :: Replay)
{-# DEPRECATED repEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplayStartTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
repReplayStartTime = Lens.lens (replayStartTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayStartTime = a} :: Replay)
{-# DEPRECATED repReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | A time stamp for the time that the replay completed.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplayEndTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
repReplayEndTime = Lens.lens (replayEndTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayEndTime = a} :: Replay)
{-# DEPRECATED repReplayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead." #-}

-- | A time stamp for the time that the last event was replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repEventLastReplayedTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
repEventLastReplayedTime = Lens.lens (eventLastReplayedTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventLastReplayedTime = a} :: Replay)
{-# DEPRECATED repEventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead." #-}

-- | A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repEventStartTime :: Lens.Lens' Replay (Lude.Maybe Lude.Timestamp)
repEventStartTime = Lens.lens (eventStartTime :: Replay -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventStartTime = a} :: Replay)
{-# DEPRECATED repEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplayName :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
repReplayName = Lens.lens (replayName :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {replayName = a} :: Replay)
{-# DEPRECATED repReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | A description of why the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repStateReason :: Lens.Lens' Replay (Lude.Maybe Lude.Text)
repStateReason = Lens.lens (stateReason :: Replay -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: Replay)
{-# DEPRECATED repStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

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
