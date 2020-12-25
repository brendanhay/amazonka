{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.StartReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified replay. Events are not necessarily replayed in the exact same order that they were added to the archive. A replay processes events to replay based on the time in the event, and replays them using 1 minute intervals. If you specify an @EventStartTime@ and an @EventEndTime@ that covers a 20 minute time range, the events are replayed from the first minute of that 20 minute range first. Then the events from the second minute are replayed. You can use @DescribeReplay@ to determine the progress of a replay. The value returned for @EventLastReplayedTime@ indicates the time within the specified time range associated with the last event replayed.
module Network.AWS.CloudWatchEvents.StartReplay
  ( -- * Creating a request
    StartReplay (..),
    mkStartReplay,

    -- ** Request lenses
    srReplayName,
    srEventSourceArn,
    srEventStartTime,
    srEventEndTime,
    srDestination,
    srDescription,

    -- * Destructuring the response
    StartReplayResponse (..),
    mkStartReplayResponse,

    -- ** Response lenses
    srrrsReplayArn,
    srrrsReplayStartTime,
    srrrsState,
    srrrsStateReason,
    srrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartReplay' smart constructor.
data StartReplay = StartReplay'
  { -- | The name of the replay to start.
    replayName :: Types.ReplayName,
    -- | The ARN of the archive to replay events from.
    eventSourceArn :: Types.EventSourceArn,
    -- | A time stamp for the time to start replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventStartTime :: Core.NominalDiffTime,
    -- | A time stamp for the time to stop replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventEndTime :: Core.NominalDiffTime,
    -- | A @ReplayDestination@ object that includes details about the destination for the replay.
    destination :: Types.ReplayDestination,
    -- | A description for the replay to start.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartReplay' value with any optional fields omitted.
mkStartReplay ::
  -- | 'replayName'
  Types.ReplayName ->
  -- | 'eventSourceArn'
  Types.EventSourceArn ->
  -- | 'eventStartTime'
  Core.NominalDiffTime ->
  -- | 'eventEndTime'
  Core.NominalDiffTime ->
  -- | 'destination'
  Types.ReplayDestination ->
  StartReplay
mkStartReplay
  replayName
  eventSourceArn
  eventStartTime
  eventEndTime
  destination =
    StartReplay'
      { replayName,
        eventSourceArn,
        eventStartTime,
        eventEndTime,
        destination,
        description = Core.Nothing
      }

-- | The name of the replay to start.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srReplayName :: Lens.Lens' StartReplay Types.ReplayName
srReplayName = Lens.field @"replayName"
{-# DEPRECATED srReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | The ARN of the archive to replay events from.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventSourceArn :: Lens.Lens' StartReplay Types.EventSourceArn
srEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED srEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | A time stamp for the time to start replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventStartTime :: Lens.Lens' StartReplay Core.NominalDiffTime
srEventStartTime = Lens.field @"eventStartTime"
{-# DEPRECATED srEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | A time stamp for the time to stop replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventEndTime :: Lens.Lens' StartReplay Core.NominalDiffTime
srEventEndTime = Lens.field @"eventEndTime"
{-# DEPRECATED srEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | A @ReplayDestination@ object that includes details about the destination for the replay.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDestination :: Lens.Lens' StartReplay Types.ReplayDestination
srDestination = Lens.field @"destination"
{-# DEPRECATED srDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | A description for the replay to start.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDescription :: Lens.Lens' StartReplay (Core.Maybe Types.Description)
srDescription = Lens.field @"description"
{-# DEPRECATED srDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON StartReplay where
  toJSON StartReplay {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReplayName" Core..= replayName),
            Core.Just ("EventSourceArn" Core..= eventSourceArn),
            Core.Just ("EventStartTime" Core..= eventStartTime),
            Core.Just ("EventEndTime" Core..= eventEndTime),
            Core.Just ("Destination" Core..= destination),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest StartReplay where
  type Rs StartReplay = StartReplayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.StartReplay")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplayResponse'
            Core.<$> (x Core..:? "ReplayArn")
            Core.<*> (x Core..:? "ReplayStartTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartReplayResponse' smart constructor.
data StartReplayResponse = StartReplayResponse'
  { -- | The ARN of the replay.
    replayArn :: Core.Maybe Types.ReplayArn,
    -- | The time at which the replay started.
    replayStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The state of the replay.
    state :: Core.Maybe Types.ReplayState,
    -- | The reason that the replay is in the state.
    stateReason :: Core.Maybe Types.ReplayStateReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartReplayResponse' value with any optional fields omitted.
mkStartReplayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartReplayResponse
mkStartReplayResponse responseStatus =
  StartReplayResponse'
    { replayArn = Core.Nothing,
      replayStartTime = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      responseStatus
    }

-- | The ARN of the replay.
--
-- /Note:/ Consider using 'replayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsReplayArn :: Lens.Lens' StartReplayResponse (Core.Maybe Types.ReplayArn)
srrrsReplayArn = Lens.field @"replayArn"
{-# DEPRECATED srrrsReplayArn "Use generic-lens or generic-optics with 'replayArn' instead." #-}

-- | The time at which the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsReplayStartTime :: Lens.Lens' StartReplayResponse (Core.Maybe Core.NominalDiffTime)
srrrsReplayStartTime = Lens.field @"replayStartTime"
{-# DEPRECATED srrrsReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | The state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsState :: Lens.Lens' StartReplayResponse (Core.Maybe Types.ReplayState)
srrrsState = Lens.field @"state"
{-# DEPRECATED srrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason that the replay is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsStateReason :: Lens.Lens' StartReplayResponse (Core.Maybe Types.ReplayStateReason)
srrrsStateReason = Lens.field @"stateReason"
{-# DEPRECATED srrrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResponseStatus :: Lens.Lens' StartReplayResponse Core.Int
srrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
