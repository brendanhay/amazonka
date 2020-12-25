{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeReplay (..),
    mkDescribeReplay,

    -- ** Request lenses
    drReplayName,

    -- * Destructuring the response
    DescribeReplayResponse (..),
    mkDescribeReplayResponse,

    -- ** Response lenses
    drsDescription,
    drsDestination,
    drsEventEndTime,
    drsEventLastReplayedTime,
    drsEventSourceArn,
    drsEventStartTime,
    drsReplayArn,
    drsReplayEndTime,
    drsReplayName,
    drsReplayStartTime,
    drsState,
    drsStateReason,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeReplay' smart constructor.
newtype DescribeReplay = DescribeReplay'
  { -- | The name of the replay to retrieve.
    replayName :: Types.ReplayName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplay' value with any optional fields omitted.
mkDescribeReplay ::
  -- | 'replayName'
  Types.ReplayName ->
  DescribeReplay
mkDescribeReplay replayName = DescribeReplay' {replayName}

-- | The name of the replay to retrieve.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drReplayName :: Lens.Lens' DescribeReplay Types.ReplayName
drReplayName = Lens.field @"replayName"
{-# DEPRECATED drReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

instance Core.FromJSON DescribeReplay where
  toJSON DescribeReplay {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ReplayName" Core..= replayName)])

instance Core.AWSRequest DescribeReplay where
  type Rs DescribeReplay = DescribeReplayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribeReplay")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplayResponse'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..:? "Destination")
            Core.<*> (x Core..:? "EventEndTime")
            Core.<*> (x Core..:? "EventLastReplayedTime")
            Core.<*> (x Core..:? "EventSourceArn")
            Core.<*> (x Core..:? "EventStartTime")
            Core.<*> (x Core..:? "ReplayArn")
            Core.<*> (x Core..:? "ReplayEndTime")
            Core.<*> (x Core..:? "ReplayName")
            Core.<*> (x Core..:? "ReplayStartTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { -- | The description of the replay.
    description :: Core.Maybe Types.ReplayDescription,
    -- | A @ReplayDestination@ object that contains details about the replay.
    destination :: Core.Maybe Types.ReplayDestination,
    -- | The time stamp for the last event that was replayed from the archive.
    eventEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time that the event was last replayed.
    eventLastReplayedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the archive events were replayed from.
    eventSourceArn :: Core.Maybe Types.Arn,
    -- | The time stamp of the first event that was last replayed from the archive.
    eventStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the replay.
    replayArn :: Core.Maybe Types.ReplayArn,
    -- | A time stamp for the time that the replay stopped.
    replayEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the replay.
    replayName :: Core.Maybe Types.ReplayName,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The current state of the replay.
    state :: Core.Maybe Types.ReplayState,
    -- | The reason that the replay is in the current state.
    stateReason :: Core.Maybe Types.ReplayStateReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeReplayResponse' value with any optional fields omitted.
mkDescribeReplayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReplayResponse
mkDescribeReplayResponse responseStatus =
  DescribeReplayResponse'
    { description = Core.Nothing,
      destination = Core.Nothing,
      eventEndTime = Core.Nothing,
      eventLastReplayedTime = Core.Nothing,
      eventSourceArn = Core.Nothing,
      eventStartTime = Core.Nothing,
      replayArn = Core.Nothing,
      replayEndTime = Core.Nothing,
      replayName = Core.Nothing,
      replayStartTime = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      responseStatus
    }

-- | The description of the replay.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayDescription)
drsDescription = Lens.field @"description"
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A @ReplayDestination@ object that contains details about the replay.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDestination :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayDestination)
drsDestination = Lens.field @"destination"
{-# DEPRECATED drsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The time stamp for the last event that was replayed from the archive.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventEndTime = Lens.field @"eventEndTime"
{-# DEPRECATED drsEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | The time that the event was last replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventLastReplayedTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventLastReplayedTime = Lens.field @"eventLastReplayedTime"
{-# DEPRECATED drsEventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead." #-}

-- | The ARN of the archive events were replayed from.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventSourceArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.Arn)
drsEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED drsEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | The time stamp of the first event that was last replayed from the archive.
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsEventStartTime = Lens.field @"eventStartTime"
{-# DEPRECATED drsEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | The ARN of the replay.
--
-- /Note:/ Consider using 'replayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayArn)
drsReplayArn = Lens.field @"replayArn"
{-# DEPRECATED drsReplayArn "Use generic-lens or generic-optics with 'replayArn' instead." #-}

-- | A time stamp for the time that the replay stopped.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsReplayEndTime = Lens.field @"replayEndTime"
{-# DEPRECATED drsReplayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead." #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayName :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayName)
drsReplayName = Lens.field @"replayName"
{-# DEPRECATED drsReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.NominalDiffTime)
drsReplayStartTime = Lens.field @"replayStartTime"
{-# DEPRECATED drsReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsState :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayState)
drsState = Lens.field @"state"
{-# DEPRECATED drsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStateReason :: Lens.Lens' DescribeReplayResponse (Core.Maybe Types.ReplayStateReason)
drsStateReason = Lens.field @"stateReason"
{-# DEPRECATED drsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeReplayResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
