{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drsEventSourceARN,
    drsDestination,
    drsState,
    drsEventEndTime,
    drsReplayStartTime,
    drsReplayARN,
    drsReplayEndTime,
    drsEventLastReplayedTime,
    drsEventStartTime,
    drsReplayName,
    drsStateReason,
    drsDescription,
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeReplay' smart constructor.
newtype DescribeReplay = DescribeReplay' {replayName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplay' with the minimum fields required to make a request.
--
-- * 'replayName' - The name of the replay to retrieve.
mkDescribeReplay ::
  -- | 'replayName'
  Lude.Text ->
  DescribeReplay
mkDescribeReplay pReplayName_ =
  DescribeReplay' {replayName = pReplayName_}

-- | The name of the replay to retrieve.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drReplayName :: Lens.Lens' DescribeReplay Lude.Text
drReplayName = Lens.lens (replayName :: DescribeReplay -> Lude.Text) (\s a -> s {replayName = a} :: DescribeReplay)
{-# DEPRECATED drReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

instance Lude.AWSRequest DescribeReplay where
  type Rs DescribeReplay = DescribeReplayResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplayResponse'
            Lude.<$> (x Lude..?> "EventSourceArn")
            Lude.<*> (x Lude..?> "Destination")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "EventEndTime")
            Lude.<*> (x Lude..?> "ReplayStartTime")
            Lude.<*> (x Lude..?> "ReplayArn")
            Lude.<*> (x Lude..?> "ReplayEndTime")
            Lude.<*> (x Lude..?> "EventLastReplayedTime")
            Lude.<*> (x Lude..?> "EventStartTime")
            Lude.<*> (x Lude..?> "ReplayName")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplay where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribeReplay" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplay where
  toJSON DescribeReplay' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ReplayName" Lude..= replayName)])

instance Lude.ToPath DescribeReplay where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplay where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { eventSourceARN ::
      Lude.Maybe Lude.Text,
    destination :: Lude.Maybe ReplayDestination,
    state :: Lude.Maybe ReplayState,
    eventEndTime :: Lude.Maybe Lude.Timestamp,
    replayStartTime :: Lude.Maybe Lude.Timestamp,
    replayARN :: Lude.Maybe Lude.Text,
    replayEndTime :: Lude.Maybe Lude.Timestamp,
    eventLastReplayedTime ::
      Lude.Maybe Lude.Timestamp,
    eventStartTime :: Lude.Maybe Lude.Timestamp,
    replayName :: Lude.Maybe Lude.Text,
    stateReason :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplayResponse' with the minimum fields required to make a request.
--
-- * 'description' - The description of the replay.
-- * 'destination' - A @ReplayDestination@ object that contains details about the replay.
-- * 'eventEndTime' - The time stamp for the last event that was replayed from the archive.
-- * 'eventLastReplayedTime' - The time that the event was last replayed.
-- * 'eventSourceARN' - The ARN of the archive events were replayed from.
-- * 'eventStartTime' - The time stamp of the first event that was last replayed from the archive.
-- * 'replayARN' - The ARN of the replay.
-- * 'replayEndTime' - A time stamp for the time that the replay stopped.
-- * 'replayName' - The name of the replay.
-- * 'replayStartTime' - A time stamp for the time that the replay started.
-- * 'responseStatus' - The response status code.
-- * 'state' - The current state of the replay.
-- * 'stateReason' - The reason that the replay is in the current state.
mkDescribeReplayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplayResponse
mkDescribeReplayResponse pResponseStatus_ =
  DescribeReplayResponse'
    { eventSourceARN = Lude.Nothing,
      destination = Lude.Nothing,
      state = Lude.Nothing,
      eventEndTime = Lude.Nothing,
      replayStartTime = Lude.Nothing,
      replayARN = Lude.Nothing,
      replayEndTime = Lude.Nothing,
      eventLastReplayedTime = Lude.Nothing,
      eventStartTime = Lude.Nothing,
      replayName = Lude.Nothing,
      stateReason = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the archive events were replayed from.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventSourceARN :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Text)
drsEventSourceARN = Lens.lens (eventSourceARN :: DescribeReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: DescribeReplayResponse)
{-# DEPRECATED drsEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | A @ReplayDestination@ object that contains details about the replay.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDestination :: Lens.Lens' DescribeReplayResponse (Lude.Maybe ReplayDestination)
drsDestination = Lens.lens (destination :: DescribeReplayResponse -> Lude.Maybe ReplayDestination) (\s a -> s {destination = a} :: DescribeReplayResponse)
{-# DEPRECATED drsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsState :: Lens.Lens' DescribeReplayResponse (Lude.Maybe ReplayState)
drsState = Lens.lens (state :: DescribeReplayResponse -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: DescribeReplayResponse)
{-# DEPRECATED drsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time stamp for the last event that was replayed from the archive.
--
-- /Note:/ Consider using 'eventEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventEndTime :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Timestamp)
drsEventEndTime = Lens.lens (eventEndTime :: DescribeReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventEndTime = a} :: DescribeReplayResponse)
{-# DEPRECATED drsEventEndTime "Use generic-lens or generic-optics with 'eventEndTime' instead." #-}

-- | A time stamp for the time that the replay started.
--
-- /Note:/ Consider using 'replayStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayStartTime :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Timestamp)
drsReplayStartTime = Lens.lens (replayStartTime :: DescribeReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayStartTime = a} :: DescribeReplayResponse)
{-# DEPRECATED drsReplayStartTime "Use generic-lens or generic-optics with 'replayStartTime' instead." #-}

-- | The ARN of the replay.
--
-- /Note:/ Consider using 'replayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayARN :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Text)
drsReplayARN = Lens.lens (replayARN :: DescribeReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {replayARN = a} :: DescribeReplayResponse)
{-# DEPRECATED drsReplayARN "Use generic-lens or generic-optics with 'replayARN' instead." #-}

-- | A time stamp for the time that the replay stopped.
--
-- /Note:/ Consider using 'replayEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayEndTime :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Timestamp)
drsReplayEndTime = Lens.lens (replayEndTime :: DescribeReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {replayEndTime = a} :: DescribeReplayResponse)
{-# DEPRECATED drsReplayEndTime "Use generic-lens or generic-optics with 'replayEndTime' instead." #-}

-- | The time that the event was last replayed.
--
-- /Note:/ Consider using 'eventLastReplayedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventLastReplayedTime :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Timestamp)
drsEventLastReplayedTime = Lens.lens (eventLastReplayedTime :: DescribeReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventLastReplayedTime = a} :: DescribeReplayResponse)
{-# DEPRECATED drsEventLastReplayedTime "Use generic-lens or generic-optics with 'eventLastReplayedTime' instead." #-}

-- | The time stamp of the first event that was last replayed from the archive.
--
-- /Note:/ Consider using 'eventStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventStartTime :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Timestamp)
drsEventStartTime = Lens.lens (eventStartTime :: DescribeReplayResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventStartTime = a} :: DescribeReplayResponse)
{-# DEPRECATED drsEventStartTime "Use generic-lens or generic-optics with 'eventStartTime' instead." #-}

-- | The name of the replay.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReplayName :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Text)
drsReplayName = Lens.lens (replayName :: DescribeReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {replayName = a} :: DescribeReplayResponse)
{-# DEPRECATED drsReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStateReason :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Text)
drsStateReason = Lens.lens (stateReason :: DescribeReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: DescribeReplayResponse)
{-# DEPRECATED drsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The description of the replay.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeReplayResponse (Lude.Maybe Lude.Text)
drsDescription = Lens.lens (description :: DescribeReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeReplayResponse)
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeReplayResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeReplayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplayResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
