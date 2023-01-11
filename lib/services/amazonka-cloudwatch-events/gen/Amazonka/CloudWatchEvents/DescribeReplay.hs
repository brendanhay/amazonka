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
-- Module      : Amazonka.CloudWatchEvents.DescribeReplay
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudWatchEvents.DescribeReplay
  ( -- * Creating a Request
    DescribeReplay (..),
    newDescribeReplay,

    -- * Request Lenses
    describeReplay_replayName,

    -- * Destructuring the Response
    DescribeReplayResponse (..),
    newDescribeReplayResponse,

    -- * Response Lenses
    describeReplayResponse_description,
    describeReplayResponse_destination,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_replayArn,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_replayName,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_state,
    describeReplayResponse_stateReason,
    describeReplayResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReplay' smart constructor.
data DescribeReplay = DescribeReplay'
  { -- | The name of the replay to retrieve.
    replayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeReplay
newDescribeReplay pReplayName_ =
  DescribeReplay' {replayName = pReplayName_}

-- | The name of the replay to retrieve.
describeReplay_replayName :: Lens.Lens' DescribeReplay Prelude.Text
describeReplay_replayName = Lens.lens (\DescribeReplay' {replayName} -> replayName) (\s@DescribeReplay' {} a -> s {replayName = a} :: DescribeReplay)

instance Core.AWSRequest DescribeReplay where
  type
    AWSResponse DescribeReplay =
      DescribeReplayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplayResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Destination")
            Prelude.<*> (x Data..?> "EventEndTime")
            Prelude.<*> (x Data..?> "EventLastReplayedTime")
            Prelude.<*> (x Data..?> "EventSourceArn")
            Prelude.<*> (x Data..?> "EventStartTime")
            Prelude.<*> (x Data..?> "ReplayArn")
            Prelude.<*> (x Data..?> "ReplayEndTime")
            Prelude.<*> (x Data..?> "ReplayName")
            Prelude.<*> (x Data..?> "ReplayStartTime")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReplay where
  hashWithSalt _salt DescribeReplay' {..} =
    _salt `Prelude.hashWithSalt` replayName

instance Prelude.NFData DescribeReplay where
  rnf DescribeReplay' {..} = Prelude.rnf replayName

instance Data.ToHeaders DescribeReplay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DescribeReplay" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReplay where
  toJSON DescribeReplay' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ReplayName" Data..= replayName)]
      )

instance Data.ToPath DescribeReplay where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReplay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { -- | The description of the replay.
    description :: Prelude.Maybe Prelude.Text,
    -- | A @ReplayDestination@ object that contains details about the replay.
    destination :: Prelude.Maybe ReplayDestination,
    -- | The time stamp for the last event that was replayed from the archive.
    eventEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the event was last replayed.
    eventLastReplayedTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the archive events were replayed from.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the first event that was last replayed from the
    -- archive.
    eventStartTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the replay.
    replayArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the replay stopped.
    replayEndTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the replay.
    replayName :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Prelude.Maybe Data.POSIX,
    -- | The current state of the replay.
    state :: Prelude.Maybe ReplayState,
    -- | The reason that the replay is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeReplayResponse_description' - The description of the replay.
--
-- 'destination', 'describeReplayResponse_destination' - A @ReplayDestination@ object that contains details about the replay.
--
-- 'eventEndTime', 'describeReplayResponse_eventEndTime' - The time stamp for the last event that was replayed from the archive.
--
-- 'eventLastReplayedTime', 'describeReplayResponse_eventLastReplayedTime' - The time that the event was last replayed.
--
-- 'eventSourceArn', 'describeReplayResponse_eventSourceArn' - The ARN of the archive events were replayed from.
--
-- 'eventStartTime', 'describeReplayResponse_eventStartTime' - The time stamp of the first event that was last replayed from the
-- archive.
--
-- 'replayArn', 'describeReplayResponse_replayArn' - The ARN of the replay.
--
-- 'replayEndTime', 'describeReplayResponse_replayEndTime' - A time stamp for the time that the replay stopped.
--
-- 'replayName', 'describeReplayResponse_replayName' - The name of the replay.
--
-- 'replayStartTime', 'describeReplayResponse_replayStartTime' - A time stamp for the time that the replay started.
--
-- 'state', 'describeReplayResponse_state' - The current state of the replay.
--
-- 'stateReason', 'describeReplayResponse_stateReason' - The reason that the replay is in the current state.
--
-- 'httpStatus', 'describeReplayResponse_httpStatus' - The response's http status code.
newDescribeReplayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplayResponse
newDescribeReplayResponse pHttpStatus_ =
  DescribeReplayResponse'
    { description =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      eventEndTime = Prelude.Nothing,
      eventLastReplayedTime = Prelude.Nothing,
      eventSourceArn = Prelude.Nothing,
      eventStartTime = Prelude.Nothing,
      replayArn = Prelude.Nothing,
      replayEndTime = Prelude.Nothing,
      replayName = Prelude.Nothing,
      replayStartTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the replay.
describeReplayResponse_description :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.Text)
describeReplayResponse_description = Lens.lens (\DescribeReplayResponse' {description} -> description) (\s@DescribeReplayResponse' {} a -> s {description = a} :: DescribeReplayResponse)

-- | A @ReplayDestination@ object that contains details about the replay.
describeReplayResponse_destination :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe ReplayDestination)
describeReplayResponse_destination = Lens.lens (\DescribeReplayResponse' {destination} -> destination) (\s@DescribeReplayResponse' {} a -> s {destination = a} :: DescribeReplayResponse)

-- | The time stamp for the last event that was replayed from the archive.
describeReplayResponse_eventEndTime :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.UTCTime)
describeReplayResponse_eventEndTime = Lens.lens (\DescribeReplayResponse' {eventEndTime} -> eventEndTime) (\s@DescribeReplayResponse' {} a -> s {eventEndTime = a} :: DescribeReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The time that the event was last replayed.
describeReplayResponse_eventLastReplayedTime :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.UTCTime)
describeReplayResponse_eventLastReplayedTime = Lens.lens (\DescribeReplayResponse' {eventLastReplayedTime} -> eventLastReplayedTime) (\s@DescribeReplayResponse' {} a -> s {eventLastReplayedTime = a} :: DescribeReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the archive events were replayed from.
describeReplayResponse_eventSourceArn :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.Text)
describeReplayResponse_eventSourceArn = Lens.lens (\DescribeReplayResponse' {eventSourceArn} -> eventSourceArn) (\s@DescribeReplayResponse' {} a -> s {eventSourceArn = a} :: DescribeReplayResponse)

-- | The time stamp of the first event that was last replayed from the
-- archive.
describeReplayResponse_eventStartTime :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.UTCTime)
describeReplayResponse_eventStartTime = Lens.lens (\DescribeReplayResponse' {eventStartTime} -> eventStartTime) (\s@DescribeReplayResponse' {} a -> s {eventStartTime = a} :: DescribeReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the replay.
describeReplayResponse_replayArn :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.Text)
describeReplayResponse_replayArn = Lens.lens (\DescribeReplayResponse' {replayArn} -> replayArn) (\s@DescribeReplayResponse' {} a -> s {replayArn = a} :: DescribeReplayResponse)

-- | A time stamp for the time that the replay stopped.
describeReplayResponse_replayEndTime :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.UTCTime)
describeReplayResponse_replayEndTime = Lens.lens (\DescribeReplayResponse' {replayEndTime} -> replayEndTime) (\s@DescribeReplayResponse' {} a -> s {replayEndTime = a} :: DescribeReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the replay.
describeReplayResponse_replayName :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.Text)
describeReplayResponse_replayName = Lens.lens (\DescribeReplayResponse' {replayName} -> replayName) (\s@DescribeReplayResponse' {} a -> s {replayName = a} :: DescribeReplayResponse)

-- | A time stamp for the time that the replay started.
describeReplayResponse_replayStartTime :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.UTCTime)
describeReplayResponse_replayStartTime = Lens.lens (\DescribeReplayResponse' {replayStartTime} -> replayStartTime) (\s@DescribeReplayResponse' {} a -> s {replayStartTime = a} :: DescribeReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The current state of the replay.
describeReplayResponse_state :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe ReplayState)
describeReplayResponse_state = Lens.lens (\DescribeReplayResponse' {state} -> state) (\s@DescribeReplayResponse' {} a -> s {state = a} :: DescribeReplayResponse)

-- | The reason that the replay is in the current state.
describeReplayResponse_stateReason :: Lens.Lens' DescribeReplayResponse (Prelude.Maybe Prelude.Text)
describeReplayResponse_stateReason = Lens.lens (\DescribeReplayResponse' {stateReason} -> stateReason) (\s@DescribeReplayResponse' {} a -> s {stateReason = a} :: DescribeReplayResponse)

-- | The response's http status code.
describeReplayResponse_httpStatus :: Lens.Lens' DescribeReplayResponse Prelude.Int
describeReplayResponse_httpStatus = Lens.lens (\DescribeReplayResponse' {httpStatus} -> httpStatus) (\s@DescribeReplayResponse' {} a -> s {httpStatus = a} :: DescribeReplayResponse)

instance Prelude.NFData DescribeReplayResponse where
  rnf DescribeReplayResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf eventEndTime
      `Prelude.seq` Prelude.rnf eventLastReplayedTime
      `Prelude.seq` Prelude.rnf eventSourceArn
      `Prelude.seq` Prelude.rnf eventStartTime
      `Prelude.seq` Prelude.rnf replayArn
      `Prelude.seq` Prelude.rnf replayEndTime
      `Prelude.seq` Prelude.rnf replayName
      `Prelude.seq` Prelude.rnf replayStartTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf httpStatus
