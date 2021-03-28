{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Replay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Replay
  ( Replay (..)
  -- * Smart constructor
  , mkReplay
  -- * Lenses
  , rfEventEndTime
  , rfEventLastReplayedTime
  , rfEventSourceArn
  , rfEventStartTime
  , rfReplayEndTime
  , rfReplayName
  , rfReplayStartTime
  , rfState
  , rfStateReason
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.Arn as Types
import qualified Network.AWS.CloudWatchEvents.Types.ReplayName as Types
import qualified Network.AWS.CloudWatchEvents.Types.ReplayState as Types
import qualified Network.AWS.CloudWatchEvents.Types.ReplayStateReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @Replay@ object that contains details about a replay.
--
-- /See:/ 'mkReplay' smart constructor.
data Replay = Replay'
  { eventEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
  , eventLastReplayedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time that the last event was replayed.
  , eventSourceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the archive to replay event from.
  , eventStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
  , replayEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time that the replay completed.
  , replayName :: Core.Maybe Types.ReplayName
    -- ^ The name of the replay.
  , replayStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time that the replay started.
  , state :: Core.Maybe Types.ReplayState
    -- ^ The current state of the replay.
  , stateReason :: Core.Maybe Types.ReplayStateReason
    -- ^ A description of why the replay is in the current state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Replay' value with any optional fields omitted.
mkReplay
    :: Replay
mkReplay
  = Replay'{eventEndTime = Core.Nothing,
            eventLastReplayedTime = Core.Nothing,
            eventSourceArn = Core.Nothing, eventStartTime = Core.Nothing,
            replayEndTime = Core.Nothing, replayName = Core.Nothing,
            replayStartTime = Core.Nothing, state = Core.Nothing,
            stateReason = Core.Nothing}

-- | A time stamp for the time to start replaying events. Any event with a creation time prior to the @EventEndTime@ specified is replayed.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventEndTime :: Lens.Lens' Replay (Core.Maybe Core.NominalDiffTime)
rfEventEndTime = Lens.field @"eventEndTime"
{-# INLINEABLE rfEventEndTime #-}
{-# DEPRECATED eventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead"  #-}

-- | A time stamp for the time that the last event was replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventLastReplayedTime :: Lens.Lens' Replay (Core.Maybe Core.NominalDiffTime)
rfEventLastReplayedTime = Lens.field @"eventLastReplayedTime"
{-# INLINEABLE rfEventLastReplayedTime #-}
{-# DEPRECATED eventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead"  #-}

-- | The ARN of the archive to replay event from.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventSourceArn :: Lens.Lens' Replay (Core.Maybe Types.Arn)
rfEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE rfEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | A time stamp for the time to start replaying events. This is determined by the time in the event as described in <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time> .
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfEventStartTime :: Lens.Lens' Replay (Core.Maybe Core.NominalDiffTime)
rfEventStartTime = Lens.field @"eventStartTime"
{-# INLINEABLE rfEventStartTime #-}
{-# DEPRECATED eventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead"  #-}

-- | A time stamp for the time that the replay completed.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayEndTime :: Lens.Lens' Replay (Core.Maybe Core.NominalDiffTime)
rfReplayEndTime = Lens.field @"replayEndTime"
{-# INLINEABLE rfReplayEndTime #-}
{-# DEPRECATED replayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead"  #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayName :: Lens.Lens' Replay (Core.Maybe Types.ReplayName)
rfReplayName = Lens.field @"replayName"
{-# INLINEABLE rfReplayName #-}
{-# DEPRECATED replayName "Use generic-lens or generic-optics with 'replayName' instead"  #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfReplayStartTime :: Lens.Lens' Replay (Core.Maybe Core.NominalDiffTime)
rfReplayStartTime = Lens.field @"replayStartTime"
{-# INLINEABLE rfReplayStartTime #-}
{-# DEPRECATED replayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead"  #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfState :: Lens.Lens' Replay (Core.Maybe Types.ReplayState)
rfState = Lens.field @"state"
{-# INLINEABLE rfState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A description of why the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfStateReason :: Lens.Lens' Replay (Core.Maybe Types.ReplayStateReason)
rfStateReason = Lens.field @"stateReason"
{-# INLINEABLE rfStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

instance Core.FromJSON Replay where
        parseJSON
          = Core.withObject "Replay" Core.$
              \ x ->
                Replay' Core.<$>
                  (x Core..:? "EventEndTime") Core.<*>
                    x Core..:? "EventLastReplayedTime"
                    Core.<*> x Core..:? "EventSourceArn"
                    Core.<*> x Core..:? "EventStartTime"
                    Core.<*> x Core..:? "ReplayEndTime"
                    Core.<*> x Core..:? "ReplayName"
                    Core.<*> x Core..:? "ReplayStartTime"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateReason"
