{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.StartReplay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified replay. Events are not necessarily replayed in the
-- exact same order that they were added to the archive. A replay processes
-- events to replay based on the time in the event, and replays them using
-- 1 minute intervals. If you specify an @EventStartTime@ and an
-- @EventEndTime@ that covers a 20 minute time range, the events are
-- replayed from the first minute of that 20 minute range first. Then the
-- events from the second minute are replayed. You can use @DescribeReplay@
-- to determine the progress of a replay. The value returned for
-- @EventLastReplayedTime@ indicates the time within the specified time
-- range associated with the last event replayed.
module Network.AWS.CloudWatchEvents.StartReplay
  ( -- * Creating a Request
    StartReplay (..),
    newStartReplay,

    -- * Request Lenses
    startReplay_description,
    startReplay_replayName,
    startReplay_eventSourceArn,
    startReplay_eventStartTime,
    startReplay_eventEndTime,
    startReplay_destination,

    -- * Destructuring the Response
    StartReplayResponse (..),
    newStartReplayResponse,

    -- * Response Lenses
    startReplayResponse_replayStartTime,
    startReplayResponse_replayArn,
    startReplayResponse_stateReason,
    startReplayResponse_state,
    startReplayResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartReplay' smart constructor.
data StartReplay = StartReplay'
  { -- | A description for the replay to start.
    description :: Core.Maybe Core.Text,
    -- | The name of the replay to start.
    replayName :: Core.Text,
    -- | The ARN of the archive to replay events from.
    eventSourceArn :: Core.Text,
    -- | A time stamp for the time to start replaying events. Only events that
    -- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventStartTime :: Core.POSIX,
    -- | A time stamp for the time to stop replaying events. Only events that
    -- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventEndTime :: Core.POSIX,
    -- | A @ReplayDestination@ object that includes details about the destination
    -- for the replay.
    destination :: ReplayDestination
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartReplay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startReplay_description' - A description for the replay to start.
--
-- 'replayName', 'startReplay_replayName' - The name of the replay to start.
--
-- 'eventSourceArn', 'startReplay_eventSourceArn' - The ARN of the archive to replay events from.
--
-- 'eventStartTime', 'startReplay_eventStartTime' - A time stamp for the time to start replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- 'eventEndTime', 'startReplay_eventEndTime' - A time stamp for the time to stop replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
--
-- 'destination', 'startReplay_destination' - A @ReplayDestination@ object that includes details about the destination
-- for the replay.
newStartReplay ::
  -- | 'replayName'
  Core.Text ->
  -- | 'eventSourceArn'
  Core.Text ->
  -- | 'eventStartTime'
  Core.UTCTime ->
  -- | 'eventEndTime'
  Core.UTCTime ->
  -- | 'destination'
  ReplayDestination ->
  StartReplay
newStartReplay
  pReplayName_
  pEventSourceArn_
  pEventStartTime_
  pEventEndTime_
  pDestination_ =
    StartReplay'
      { description = Core.Nothing,
        replayName = pReplayName_,
        eventSourceArn = pEventSourceArn_,
        eventStartTime = Core._Time Lens.# pEventStartTime_,
        eventEndTime = Core._Time Lens.# pEventEndTime_,
        destination = pDestination_
      }

-- | A description for the replay to start.
startReplay_description :: Lens.Lens' StartReplay (Core.Maybe Core.Text)
startReplay_description = Lens.lens (\StartReplay' {description} -> description) (\s@StartReplay' {} a -> s {description = a} :: StartReplay)

-- | The name of the replay to start.
startReplay_replayName :: Lens.Lens' StartReplay Core.Text
startReplay_replayName = Lens.lens (\StartReplay' {replayName} -> replayName) (\s@StartReplay' {} a -> s {replayName = a} :: StartReplay)

-- | The ARN of the archive to replay events from.
startReplay_eventSourceArn :: Lens.Lens' StartReplay Core.Text
startReplay_eventSourceArn = Lens.lens (\StartReplay' {eventSourceArn} -> eventSourceArn) (\s@StartReplay' {} a -> s {eventSourceArn = a} :: StartReplay)

-- | A time stamp for the time to start replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
startReplay_eventStartTime :: Lens.Lens' StartReplay Core.UTCTime
startReplay_eventStartTime = Lens.lens (\StartReplay' {eventStartTime} -> eventStartTime) (\s@StartReplay' {} a -> s {eventStartTime = a} :: StartReplay) Core.. Core._Time

-- | A time stamp for the time to stop replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
startReplay_eventEndTime :: Lens.Lens' StartReplay Core.UTCTime
startReplay_eventEndTime = Lens.lens (\StartReplay' {eventEndTime} -> eventEndTime) (\s@StartReplay' {} a -> s {eventEndTime = a} :: StartReplay) Core.. Core._Time

-- | A @ReplayDestination@ object that includes details about the destination
-- for the replay.
startReplay_destination :: Lens.Lens' StartReplay ReplayDestination
startReplay_destination = Lens.lens (\StartReplay' {destination} -> destination) (\s@StartReplay' {} a -> s {destination = a} :: StartReplay)

instance Core.AWSRequest StartReplay where
  type AWSResponse StartReplay = StartReplayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplayResponse'
            Core.<$> (x Core..?> "ReplayStartTime")
            Core.<*> (x Core..?> "ReplayArn")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartReplay

instance Core.NFData StartReplay

instance Core.ToHeaders StartReplay where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.StartReplay" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartReplay where
  toJSON StartReplay' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just ("ReplayName" Core..= replayName),
            Core.Just ("EventSourceArn" Core..= eventSourceArn),
            Core.Just ("EventStartTime" Core..= eventStartTime),
            Core.Just ("EventEndTime" Core..= eventEndTime),
            Core.Just ("Destination" Core..= destination)
          ]
      )

instance Core.ToPath StartReplay where
  toPath = Core.const "/"

instance Core.ToQuery StartReplay where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartReplayResponse' smart constructor.
data StartReplayResponse = StartReplayResponse'
  { -- | The time at which the replay started.
    replayStartTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the replay.
    replayArn :: Core.Maybe Core.Text,
    -- | The reason that the replay is in the state.
    stateReason :: Core.Maybe Core.Text,
    -- | The state of the replay.
    state :: Core.Maybe ReplayState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replayStartTime', 'startReplayResponse_replayStartTime' - The time at which the replay started.
--
-- 'replayArn', 'startReplayResponse_replayArn' - The ARN of the replay.
--
-- 'stateReason', 'startReplayResponse_stateReason' - The reason that the replay is in the state.
--
-- 'state', 'startReplayResponse_state' - The state of the replay.
--
-- 'httpStatus', 'startReplayResponse_httpStatus' - The response's http status code.
newStartReplayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartReplayResponse
newStartReplayResponse pHttpStatus_ =
  StartReplayResponse'
    { replayStartTime =
        Core.Nothing,
      replayArn = Core.Nothing,
      stateReason = Core.Nothing,
      state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the replay started.
startReplayResponse_replayStartTime :: Lens.Lens' StartReplayResponse (Core.Maybe Core.UTCTime)
startReplayResponse_replayStartTime = Lens.lens (\StartReplayResponse' {replayStartTime} -> replayStartTime) (\s@StartReplayResponse' {} a -> s {replayStartTime = a} :: StartReplayResponse) Core.. Lens.mapping Core._Time

-- | The ARN of the replay.
startReplayResponse_replayArn :: Lens.Lens' StartReplayResponse (Core.Maybe Core.Text)
startReplayResponse_replayArn = Lens.lens (\StartReplayResponse' {replayArn} -> replayArn) (\s@StartReplayResponse' {} a -> s {replayArn = a} :: StartReplayResponse)

-- | The reason that the replay is in the state.
startReplayResponse_stateReason :: Lens.Lens' StartReplayResponse (Core.Maybe Core.Text)
startReplayResponse_stateReason = Lens.lens (\StartReplayResponse' {stateReason} -> stateReason) (\s@StartReplayResponse' {} a -> s {stateReason = a} :: StartReplayResponse)

-- | The state of the replay.
startReplayResponse_state :: Lens.Lens' StartReplayResponse (Core.Maybe ReplayState)
startReplayResponse_state = Lens.lens (\StartReplayResponse' {state} -> state) (\s@StartReplayResponse' {} a -> s {state = a} :: StartReplayResponse)

-- | The response's http status code.
startReplayResponse_httpStatus :: Lens.Lens' StartReplayResponse Core.Int
startReplayResponse_httpStatus = Lens.lens (\StartReplayResponse' {httpStatus} -> httpStatus) (\s@StartReplayResponse' {} a -> s {httpStatus = a} :: StartReplayResponse)

instance Core.NFData StartReplayResponse
