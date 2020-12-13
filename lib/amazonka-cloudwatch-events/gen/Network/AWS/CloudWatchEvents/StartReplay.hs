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
    srEventSourceARN,
    srDestination,
    srEventEndTime,
    srEventStartTime,
    srReplayName,
    srDescription,

    -- * Destructuring the response
    StartReplayResponse (..),
    mkStartReplayResponse,

    -- ** Response lenses
    srrsState,
    srrsReplayStartTime,
    srrsReplayARN,
    srrsStateReason,
    srrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartReplay' smart constructor.
data StartReplay = StartReplay'
  { -- | The ARN of the archive to replay events from.
    eventSourceARN :: Lude.Text,
    -- | A @ReplayDestination@ object that includes details about the destination for the replay.
    destination :: ReplayDestination,
    -- | A time stamp for the time to stop replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventEndTime :: Lude.Timestamp,
    -- | A time stamp for the time to start replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventStartTime :: Lude.Timestamp,
    -- | The name of the replay to start.
    replayName :: Lude.Text,
    -- | A description for the replay to start.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplay' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the archive to replay events from.
-- * 'destination' - A @ReplayDestination@ object that includes details about the destination for the replay.
-- * 'eventEndTime' - A time stamp for the time to stop replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
-- * 'eventStartTime' - A time stamp for the time to start replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
-- * 'replayName' - The name of the replay to start.
-- * 'description' - A description for the replay to start.
mkStartReplay ::
  -- | 'eventSourceARN'
  Lude.Text ->
  -- | 'destination'
  ReplayDestination ->
  -- | 'eventEndTime'
  Lude.Timestamp ->
  -- | 'eventStartTime'
  Lude.Timestamp ->
  -- | 'replayName'
  Lude.Text ->
  StartReplay
mkStartReplay
  pEventSourceARN_
  pDestination_
  pEventEndTime_
  pEventStartTime_
  pReplayName_ =
    StartReplay'
      { eventSourceARN = pEventSourceARN_,
        destination = pDestination_,
        eventEndTime = pEventEndTime_,
        eventStartTime = pEventStartTime_,
        replayName = pReplayName_,
        description = Lude.Nothing
      }

-- | The ARN of the archive to replay events from.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventSourceARN :: Lens.Lens' StartReplay Lude.Text
srEventSourceARN = Lens.lens (eventSourceARN :: StartReplay -> Lude.Text) (\s a -> s {eventSourceARN = a} :: StartReplay)
{-# DEPRECATED srEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | A @ReplayDestination@ object that includes details about the destination for the replay.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDestination :: Lens.Lens' StartReplay ReplayDestination
srDestination = Lens.lens (destination :: StartReplay -> ReplayDestination) (\s a -> s {destination = a} :: StartReplay)
{-# DEPRECATED srDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | A time stamp for the time to stop replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventEndTime :: Lens.Lens' StartReplay Lude.Timestamp
srEventEndTime = Lens.lens (eventEndTime :: StartReplay -> Lude.Timestamp) (\s a -> s {eventEndTime = a} :: StartReplay)
{-# DEPRECATED srEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | A time stamp for the time to start replaying events. Only events that occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEventStartTime :: Lens.Lens' StartReplay Lude.Timestamp
srEventStartTime = Lens.lens (eventStartTime :: StartReplay -> Lude.Timestamp) (\s a -> s {eventStartTime = a} :: StartReplay)
{-# DEPRECATED srEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | The name of the replay to start.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srReplayName :: Lens.Lens' StartReplay Lude.Text
srReplayName = Lens.lens (replayName :: StartReplay -> Lude.Text) (\s a -> s {replayName = a} :: StartReplay)
{-# DEPRECATED srReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | A description for the replay to start.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDescription :: Lens.Lens' StartReplay (Lude.Maybe Lude.Text)
srDescription = Lens.lens (description :: StartReplay -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartReplay)
{-# DEPRECATED srDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest StartReplay where
  type Rs StartReplay = StartReplayResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartReplayResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ReplayStartTime")
            Lude.<*> (x Lude..?> "ReplayArn")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartReplay where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.StartReplay" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartReplay where
  toJSON StartReplay' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EventSourceArn" Lude..= eventSourceARN),
            Lude.Just ("Destination" Lude..= destination),
            Lude.Just ("EventEndTime" Lude..= eventEndTime),
            Lude.Just ("EventStartTime" Lude..= eventStartTime),
            Lude.Just ("ReplayName" Lude..= replayName),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath StartReplay where
  toPath = Lude.const "/"

instance Lude.ToQuery StartReplay where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartReplayResponse' smart constructor.
data StartReplayResponse = StartReplayResponse'
  { -- | The state of the replay.
    state :: Lude.Maybe ReplayState,
    -- | The time at which the replay started.
    replayStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The ARN of the replay.
    replayARN :: Lude.Maybe Lude.Text,
    -- | The reason that the replay is in the state.
    stateReason :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplayResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the replay.
-- * 'replayStartTime' - The time at which the replay started.
-- * 'replayARN' - The ARN of the replay.
-- * 'stateReason' - The reason that the replay is in the state.
-- * 'responseStatus' - The response status code.
mkStartReplayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartReplayResponse
mkStartReplayResponse pResponseStatus_ =
  StartReplayResponse'
    { state = Lude.Nothing,
      replayStartTime = Lude.Nothing,
      replayARN = Lude.Nothing,
      stateReason = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsState :: Lens.Lens' StartReplayResponse (Lude.Maybe ReplayState)
srrsState = Lens.lens (state :: StartReplayResponse -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: StartReplayResponse)
{-# DEPRECATED srrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time at which the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsReplayStartTime :: Lens.Lens' StartReplayResponse (Lude.Maybe Lude.Timestamp)
srrsReplayStartTime = Lens.lens (replayStartTime :: StartReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayStartTime = a} :: StartReplayResponse)
{-# DEPRECATED srrsReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | The ARN of the replay.
--
-- /Note:/ Consider using 'replayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsReplayARN :: Lens.Lens' StartReplayResponse (Lude.Maybe Lude.Text)
srrsReplayARN = Lens.lens (replayARN :: StartReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {replayARN = a} :: StartReplayResponse)
{-# DEPRECATED srrsReplayARN "Use generic-lens or generic-optics with 'replayARN' instead." #-}

-- | The reason that the replay is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsStateReason :: Lens.Lens' StartReplayResponse (Lude.Maybe Lude.Text)
srrsStateReason = Lens.lens (stateReason :: StartReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: StartReplayResponse)
{-# DEPRECATED srrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' StartReplayResponse Lude.Int
srrsResponseStatus = Lens.lens (responseStatus :: StartReplayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartReplayResponse)
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
