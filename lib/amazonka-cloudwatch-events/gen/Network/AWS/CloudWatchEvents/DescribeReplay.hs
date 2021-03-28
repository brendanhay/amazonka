{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a replay. Use @DescribeReplay@ to determine the progress of a running replay. A replay processes events to replay based on the time in the event, and replays them using 1 minute intervals. If you use @StartReplay@ and specify an @EventStartTime@ and an @EventEndTime@ that covers a 20 minute time range, the events are replayed from the first minute of that 20 minute range first. Then the events from the second minute are replayed. You can use @DescribeReplay@ to determine the progress of a replay. The value returned for @EventLastReplayedTime@ indicates the time within the specified time range associated with the last event replayed.
module Network.AWS.CloudWatchEvents.DescribeReplay
    (
    -- * Creating a request
      DescribeReplay (..)
    , mkDescribeReplay
    -- ** Request lenses
    , drReplayName

    -- * Destructuring the response
    , DescribeReplayResponse (..)
    , mkDescribeReplayResponse
    -- ** Response lenses
    , drsDescription
    , drsDestination
    , drsEventEndTime
    , drsEventLastReplayedTime
    , drsEventSourceArn
    , drsEventStartTime
    , drsReplayArn
    , drsReplayEndTime
    , drsReplayName
    , drsReplayStartTime
    , drsState
    , drsStateReason
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeReplay' smart constructor.
newtype DescribeReplay = DescribeReplay'
  { replayName :: Types.ReplayName
    -- ^ The name of the replay to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplay' value with any optional fields omitted.
mkDescribeReplay
    :: Types.ReplayName -- ^ 'replayName'
    -> DescribeReplay
mkDescribeReplay replayName = DescribeReplay'{replayName}

-- | The name of the replay to retrieve.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drReplayName :: Lens.Lens' DescribeReplay Types.ReplayName
drReplayName = Lens.field @"replayName"
{-# INLINEABLE drReplayName #-}
{-# DEPRECATED replayName "Use generic-lens or generic-optics with 'replayName' instead"  #-}

instance Core.ToQuery DescribeReplay where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReplay where
        toHeaders DescribeReplay{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DescribeReplay") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplay where
        toJSON DescribeReplay{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ReplayName" Core..= replayName)])

instance Core.AWSRequest DescribeReplay where
        type Rs DescribeReplay = DescribeReplayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplayResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> x Core..:? "Destination"
                     Core.<*> x Core..:? "EventEndTime"
                     Core.<*> x Core..:? "EventLastReplayedTime"
                     Core.<*> x Core..:? "EventSourceArn"
                     Core.<*> x Core..:? "EventStartTime"
                     Core.<*> x Core..:? "ReplayArn"
                     Core.<*> x Core..:? "ReplayEndTime"
                     Core.<*> x Core..:? "ReplayName"
                     Core.<*> x Core..:? "ReplayStartTime"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "StateReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { description :: Core.Maybe Types.ReplayDescription
    -- ^ The description of the replay.
  , destination :: Core.Maybe Types.ReplayDestination
    -- ^ A @ReplayDestination@ object that contains details about the replay.
  , eventEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp for the last event that was replayed from the archive.
  , eventLastReplayedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the event was last replayed.
  , eventSourceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the archive events were replayed from.
  , eventStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp of the first event that was last replayed from the archive.
  , replayArn :: Core.Maybe Types.ReplayArn
    -- ^ The ARN of the replay.
  , replayEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time that the replay stopped.
  , replayName :: Core.Maybe Types.ReplayName
    -- ^ The name of the replay.
  , replayStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp for the time that the replay started.
  , state :: Core.Maybe Types.ReplayState
    -- ^ The current state of the replay.
  , stateReason :: Core.Maybe Types.ReplayStateReason
    -- ^ The reason that the replay is in the current state.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplayResponse' value with any optional fields omitted.
mkDescribeReplayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplayResponse
mkDescribeReplayResponse responseStatus
  = DescribeReplayResponse'{description = Core.Nothing,
                            destination = Core.Nothing, eventEndTime = Core.Nothing,
                            eventLastReplayedTime = Core.Nothing,
                            eventSourceArn = Core.Nothing, eventStartTime = Core.Nothing,
                            replayArn = Core.Nothing, replayEndTime = Core.Nothing,
                            replayName = Core.Nothing, replayStartTime = Core.Nothing,
                            state = Core.Nothing, stateReason = Core.Nothing, responseStatus}

-- | The description of the replay.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayDescription)
drsDescription = Lens.field @"description"
{-# INLINEABLE drsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A @ReplayDestination@ object that contains details about the replay.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDestination :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayDestination)
drsDestination = Lens.field @"destination"
{-# INLINEABLE drsDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The time stamp for the last event that was replayed from the archive.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventEndTime = Lens.field @"eventEndTime"
{-# INLINEABLE drsEventEndTime #-}
{-# DEPRECATED eventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead"  #-}

-- | The time that the event was last replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventLastReplayedTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventLastReplayedTime = Lens.field @"eventLastReplayedTime"
{-# INLINEABLE drsEventLastReplayedTime #-}
{-# DEPRECATED eventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead"  #-}

-- | The ARN of the archive events were replayed from.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventSourceArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.Arn)
drsEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE drsEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | The time stamp of the first event that was last replayed from the archive.
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventStartTime = Lens.field @"eventStartTime"
{-# INLINEABLE drsEventStartTime #-}
{-# DEPRECATED eventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead"  #-}

-- | The ARN of the replay.
--
-- /Note:/ Consider using 'replayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayArn)
drsReplayArn = Lens.field @"replayArn"
{-# INLINEABLE drsReplayArn #-}
{-# DEPRECATED replayArn "Use generic-lens or generic-optics with 'replayArn' instead"  #-}

-- | A time stamp for the time that the replay stopped.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsReplayEndTime = Lens.field @"replayEndTime"
{-# INLINEABLE drsReplayEndTime #-}
{-# DEPRECATED replayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead"  #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayName :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayName)
drsReplayName = Lens.field @"replayName"
{-# INLINEABLE drsReplayName #-}
{-# DEPRECATED replayName "Use generic-lens or generic-optics with 'replayName' instead"  #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsReplayStartTime = Lens.field @"replayStartTime"
{-# INLINEABLE drsReplayStartTime #-}
{-# DEPRECATED replayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead"  #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsState :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayState)
drsState = Lens.field @"state"
{-# INLINEABLE drsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStateReason :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayStateReason)
drsStateReason = Lens.field @"stateReason"
{-# INLINEABLE drsStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeReplayResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
