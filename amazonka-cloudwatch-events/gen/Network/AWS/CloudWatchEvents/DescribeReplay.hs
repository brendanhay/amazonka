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
-- Module      : Network.AWS.CloudWatchEvents.DescribeReplay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a replay. Use @DescribeReplay@ to determine the
-- progress of a running replay. A replay processes events to replay based
-- on the time in the event, and replays them using 1 minute intervals. If
-- you use @StartReplay@ and specify an @EventStartTime@ and an
-- @EventEndTime@ that covers a 20 minute time range, the events are
-- replayed from the first minute of that 20 minute range first. Then the
-- events from the second minute are replayed. You can use @DescribeReplay@
-- to determine the progress of a replay. The value returned for
-- @EventLastReplayedTime@ indicates the time within the specified time
-- range associated with the last event replayed.
module Network.AWS.CloudWatchEvents.DescribeReplay
  ( -- * Creating a Request
    DescribeReplay (..),
    newDescribeReplay,

    -- * Request Lenses
    describeReplay_replayName,

    -- * Destructuring the Response
    DescribeReplayResponse (..),
    newDescribeReplayResponse,

    -- * Response Lenses
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_replayArn,
    describeReplayResponse_stateReason,
    describeReplayResponse_state,
    describeReplayResponse_destination,
    describeReplayResponse_replayName,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_description,
    describeReplayResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeReplay' smart constructor.
data DescribeReplay = DescribeReplay'
  { -- | The name of the replay to retrieve.
    replayName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replayName', 'describeReplay_replayName' - The name of the replay to retrieve.
newDescribeReplay ::
  -- | 'replayName'
  Core.Text ->
  DescribeReplay
newDescribeReplay pReplayName_ =
  DescribeReplay' {replayName = pReplayName_}

-- | The name of the replay to retrieve.
describeReplay_replayName :: Lens.Lens' DescribeReplay Core.Text
describeReplay_replayName = Lens.lens (\DescribeReplay' {replayName} -> replayName) (\s@DescribeReplay' {} a -> s {replayName = a} :: DescribeReplay)

instance Core.AWSRequest DescribeReplay where
  type
    AWSResponse DescribeReplay =
      DescribeReplayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplayResponse'
            Core.<$> (x Core..?> "EventSourceArn")
            Core.<*> (x Core..?> "EventStartTime")
            Core.<*> (x Core..?> "ReplayStartTime")
            Core.<*> (x Core..?> "ReplayArn")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Destination")
            Core.<*> (x Core..?> "ReplayName")
            Core.<*> (x Core..?> "EventLastReplayedTime")
            Core.<*> (x Core..?> "ReplayEndTime")
            Core.<*> (x Core..?> "EventEndTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReplay

instance Core.NFData DescribeReplay

instance Core.ToHeaders DescribeReplay where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DescribeReplay" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeReplay where
  toJSON DescribeReplay' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ReplayName" Core..= replayName)]
      )

instance Core.ToPath DescribeReplay where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReplay where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { -- | The ARN of the archive events were replayed from.
    eventSourceArn :: Core.Maybe Core.Text,
    -- | The time stamp of the first event that was last replayed from the
    -- archive.
    eventStartTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the replay.
    replayArn :: Core.Maybe Core.Text,
    -- | The reason that the replay is in the current state.
    stateReason :: Core.Maybe Core.Text,
    -- | The current state of the replay.
    state :: Core.Maybe ReplayState,
    -- | A @ReplayDestination@ object that contains details about the replay.
    destination :: Core.Maybe ReplayDestination,
    -- | The name of the replay.
    replayName :: Core.Maybe Core.Text,
    -- | The time that the event was last replayed.
    eventLastReplayedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay stopped.
    replayEndTime :: Core.Maybe Core.POSIX,
    -- | The time stamp for the last event that was replayed from the archive.
    eventEndTime :: Core.Maybe Core.POSIX,
    -- | The description of the replay.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'describeReplayResponse_eventSourceArn' - The ARN of the archive events were replayed from.
--
-- 'eventStartTime', 'describeReplayResponse_eventStartTime' - The time stamp of the first event that was last replayed from the
-- archive.
--
-- 'replayStartTime', 'describeReplayResponse_replayStartTime' - A time stamp for the time that the replay started.
--
-- 'replayArn', 'describeReplayResponse_replayArn' - The ARN of the replay.
--
-- 'stateReason', 'describeReplayResponse_stateReason' - The reason that the replay is in the current state.
--
-- 'state', 'describeReplayResponse_state' - The current state of the replay.
--
-- 'destination', 'describeReplayResponse_destination' - A @ReplayDestination@ object that contains details about the replay.
--
-- 'replayName', 'describeReplayResponse_replayName' - The name of the replay.
--
-- 'eventLastReplayedTime', 'describeReplayResponse_eventLastReplayedTime' - The time that the event was last replayed.
--
-- 'replayEndTime', 'describeReplayResponse_replayEndTime' - A time stamp for the time that the replay stopped.
--
-- 'eventEndTime', 'describeReplayResponse_eventEndTime' - The time stamp for the last event that was replayed from the archive.
--
-- 'description', 'describeReplayResponse_description' - The description of the replay.
--
-- 'httpStatus', 'describeReplayResponse_httpStatus' - The response's http status code.
newDescribeReplayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReplayResponse
newDescribeReplayResponse pHttpStatus_ =
  DescribeReplayResponse'
    { eventSourceArn =
        Core.Nothing,
      eventStartTime = Core.Nothing,
      replayStartTime = Core.Nothing,
      replayArn = Core.Nothing,
      stateReason = Core.Nothing,
      state = Core.Nothing,
      destination = Core.Nothing,
      replayName = Core.Nothing,
      eventLastReplayedTime = Core.Nothing,
      replayEndTime = Core.Nothing,
      eventEndTime = Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the archive events were replayed from.
describeReplayResponse_eventSourceArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.Text)
describeReplayResponse_eventSourceArn = Lens.lens (\DescribeReplayResponse' {eventSourceArn} -> eventSourceArn) (\s@DescribeReplayResponse' {} a -> s {eventSourceArn = a} :: DescribeReplayResponse)

-- | The time stamp of the first event that was last replayed from the
-- archive.
describeReplayResponse_eventStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.UTCTime)
describeReplayResponse_eventStartTime = Lens.lens (\DescribeReplayResponse' {eventStartTime} -> eventStartTime) (\s@DescribeReplayResponse' {} a -> s {eventStartTime = a} :: DescribeReplayResponse) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay started.
describeReplayResponse_replayStartTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.UTCTime)
describeReplayResponse_replayStartTime = Lens.lens (\DescribeReplayResponse' {replayStartTime} -> replayStartTime) (\s@DescribeReplayResponse' {} a -> s {replayStartTime = a} :: DescribeReplayResponse) Core.. Lens.mapping Core._Time

-- | The ARN of the replay.
describeReplayResponse_replayArn :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.Text)
describeReplayResponse_replayArn = Lens.lens (\DescribeReplayResponse' {replayArn} -> replayArn) (\s@DescribeReplayResponse' {} a -> s {replayArn = a} :: DescribeReplayResponse)

-- | The reason that the replay is in the current state.
describeReplayResponse_stateReason :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.Text)
describeReplayResponse_stateReason = Lens.lens (\DescribeReplayResponse' {stateReason} -> stateReason) (\s@DescribeReplayResponse' {} a -> s {stateReason = a} :: DescribeReplayResponse)

-- | The current state of the replay.
describeReplayResponse_state :: Lens.Lens' DescribeReplayResponse (Core.Maybe ReplayState)
describeReplayResponse_state = Lens.lens (\DescribeReplayResponse' {state} -> state) (\s@DescribeReplayResponse' {} a -> s {state = a} :: DescribeReplayResponse)

-- | A @ReplayDestination@ object that contains details about the replay.
describeReplayResponse_destination :: Lens.Lens' DescribeReplayResponse (Core.Maybe ReplayDestination)
describeReplayResponse_destination = Lens.lens (\DescribeReplayResponse' {destination} -> destination) (\s@DescribeReplayResponse' {} a -> s {destination = a} :: DescribeReplayResponse)

-- | The name of the replay.
describeReplayResponse_replayName :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.Text)
describeReplayResponse_replayName = Lens.lens (\DescribeReplayResponse' {replayName} -> replayName) (\s@DescribeReplayResponse' {} a -> s {replayName = a} :: DescribeReplayResponse)

-- | The time that the event was last replayed.
describeReplayResponse_eventLastReplayedTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.UTCTime)
describeReplayResponse_eventLastReplayedTime = Lens.lens (\DescribeReplayResponse' {eventLastReplayedTime} -> eventLastReplayedTime) (\s@DescribeReplayResponse' {} a -> s {eventLastReplayedTime = a} :: DescribeReplayResponse) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay stopped.
describeReplayResponse_replayEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.UTCTime)
describeReplayResponse_replayEndTime = Lens.lens (\DescribeReplayResponse' {replayEndTime} -> replayEndTime) (\s@DescribeReplayResponse' {} a -> s {replayEndTime = a} :: DescribeReplayResponse) Core.. Lens.mapping Core._Time

-- | The time stamp for the last event that was replayed from the archive.
describeReplayResponse_eventEndTime :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.UTCTime)
describeReplayResponse_eventEndTime = Lens.lens (\DescribeReplayResponse' {eventEndTime} -> eventEndTime) (\s@DescribeReplayResponse' {} a -> s {eventEndTime = a} :: DescribeReplayResponse) Core.. Lens.mapping Core._Time

-- | The description of the replay.
describeReplayResponse_description :: Lens.Lens' DescribeReplayResponse (Core.Maybe Core.Text)
describeReplayResponse_description = Lens.lens (\DescribeReplayResponse' {description} -> description) (\s@DescribeReplayResponse' {} a -> s {description = a} :: DescribeReplayResponse)

-- | The response's http status code.
describeReplayResponse_httpStatus :: Lens.Lens' DescribeReplayResponse Core.Int
describeReplayResponse_httpStatus = Lens.lens (\DescribeReplayResponse' {httpStatus} -> httpStatus) (\s@DescribeReplayResponse' {} a -> s {httpStatus = a} :: DescribeReplayResponse)

instance Core.NFData DescribeReplayResponse
