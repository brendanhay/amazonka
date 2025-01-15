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
-- Module      : Amazonka.CloudWatchEvents.StartReplay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.CloudWatchEvents.StartReplay
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
    startReplayResponse_replayArn,
    startReplayResponse_replayStartTime,
    startReplayResponse_state,
    startReplayResponse_stateReason,
    startReplayResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReplay' smart constructor.
data StartReplay = StartReplay'
  { -- | A description for the replay to start.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the replay to start.
    replayName :: Prelude.Text,
    -- | The ARN of the archive to replay events from.
    eventSourceArn :: Prelude.Text,
    -- | A time stamp for the time to start replaying events. Only events that
    -- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventStartTime :: Data.POSIX,
    -- | A time stamp for the time to stop replaying events. Only events that
    -- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
    eventEndTime :: Data.POSIX,
    -- | A @ReplayDestination@ object that includes details about the destination
    -- for the replay.
    destination :: ReplayDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'eventSourceArn'
  Prelude.Text ->
  -- | 'eventStartTime'
  Prelude.UTCTime ->
  -- | 'eventEndTime'
  Prelude.UTCTime ->
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
      { description = Prelude.Nothing,
        replayName = pReplayName_,
        eventSourceArn = pEventSourceArn_,
        eventStartTime = Data._Time Lens.# pEventStartTime_,
        eventEndTime = Data._Time Lens.# pEventEndTime_,
        destination = pDestination_
      }

-- | A description for the replay to start.
startReplay_description :: Lens.Lens' StartReplay (Prelude.Maybe Prelude.Text)
startReplay_description = Lens.lens (\StartReplay' {description} -> description) (\s@StartReplay' {} a -> s {description = a} :: StartReplay)

-- | The name of the replay to start.
startReplay_replayName :: Lens.Lens' StartReplay Prelude.Text
startReplay_replayName = Lens.lens (\StartReplay' {replayName} -> replayName) (\s@StartReplay' {} a -> s {replayName = a} :: StartReplay)

-- | The ARN of the archive to replay events from.
startReplay_eventSourceArn :: Lens.Lens' StartReplay Prelude.Text
startReplay_eventSourceArn = Lens.lens (\StartReplay' {eventSourceArn} -> eventSourceArn) (\s@StartReplay' {} a -> s {eventSourceArn = a} :: StartReplay)

-- | A time stamp for the time to start replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
startReplay_eventStartTime :: Lens.Lens' StartReplay Prelude.UTCTime
startReplay_eventStartTime = Lens.lens (\StartReplay' {eventStartTime} -> eventStartTime) (\s@StartReplay' {} a -> s {eventStartTime = a} :: StartReplay) Prelude.. Data._Time

-- | A time stamp for the time to stop replaying events. Only events that
-- occurred between the @EventStartTime@ and @EventEndTime@ are replayed.
startReplay_eventEndTime :: Lens.Lens' StartReplay Prelude.UTCTime
startReplay_eventEndTime = Lens.lens (\StartReplay' {eventEndTime} -> eventEndTime) (\s@StartReplay' {} a -> s {eventEndTime = a} :: StartReplay) Prelude.. Data._Time

-- | A @ReplayDestination@ object that includes details about the destination
-- for the replay.
startReplay_destination :: Lens.Lens' StartReplay ReplayDestination
startReplay_destination = Lens.lens (\StartReplay' {destination} -> destination) (\s@StartReplay' {} a -> s {destination = a} :: StartReplay)

instance Core.AWSRequest StartReplay where
  type AWSResponse StartReplay = StartReplayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplayResponse'
            Prelude.<$> (x Data..?> "ReplayArn")
            Prelude.<*> (x Data..?> "ReplayStartTime")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartReplay where
  hashWithSalt _salt StartReplay' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` replayName
      `Prelude.hashWithSalt` eventSourceArn
      `Prelude.hashWithSalt` eventStartTime
      `Prelude.hashWithSalt` eventEndTime
      `Prelude.hashWithSalt` destination

instance Prelude.NFData StartReplay where
  rnf StartReplay' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf replayName `Prelude.seq`
        Prelude.rnf eventSourceArn `Prelude.seq`
          Prelude.rnf eventStartTime `Prelude.seq`
            Prelude.rnf eventEndTime `Prelude.seq`
              Prelude.rnf destination

instance Data.ToHeaders StartReplay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.StartReplay" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReplay where
  toJSON StartReplay' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("ReplayName" Data..= replayName),
            Prelude.Just
              ("EventSourceArn" Data..= eventSourceArn),
            Prelude.Just
              ("EventStartTime" Data..= eventStartTime),
            Prelude.Just ("EventEndTime" Data..= eventEndTime),
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath StartReplay where
  toPath = Prelude.const "/"

instance Data.ToQuery StartReplay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReplayResponse' smart constructor.
data StartReplayResponse = StartReplayResponse'
  { -- | The ARN of the replay.
    replayArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the replay started.
    replayStartTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the replay.
    state :: Prelude.Maybe ReplayState,
    -- | The reason that the replay is in the state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replayArn', 'startReplayResponse_replayArn' - The ARN of the replay.
--
-- 'replayStartTime', 'startReplayResponse_replayStartTime' - The time at which the replay started.
--
-- 'state', 'startReplayResponse_state' - The state of the replay.
--
-- 'stateReason', 'startReplayResponse_stateReason' - The reason that the replay is in the state.
--
-- 'httpStatus', 'startReplayResponse_httpStatus' - The response's http status code.
newStartReplayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReplayResponse
newStartReplayResponse pHttpStatus_ =
  StartReplayResponse'
    { replayArn = Prelude.Nothing,
      replayStartTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the replay.
startReplayResponse_replayArn :: Lens.Lens' StartReplayResponse (Prelude.Maybe Prelude.Text)
startReplayResponse_replayArn = Lens.lens (\StartReplayResponse' {replayArn} -> replayArn) (\s@StartReplayResponse' {} a -> s {replayArn = a} :: StartReplayResponse)

-- | The time at which the replay started.
startReplayResponse_replayStartTime :: Lens.Lens' StartReplayResponse (Prelude.Maybe Prelude.UTCTime)
startReplayResponse_replayStartTime = Lens.lens (\StartReplayResponse' {replayStartTime} -> replayStartTime) (\s@StartReplayResponse' {} a -> s {replayStartTime = a} :: StartReplayResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the replay.
startReplayResponse_state :: Lens.Lens' StartReplayResponse (Prelude.Maybe ReplayState)
startReplayResponse_state = Lens.lens (\StartReplayResponse' {state} -> state) (\s@StartReplayResponse' {} a -> s {state = a} :: StartReplayResponse)

-- | The reason that the replay is in the state.
startReplayResponse_stateReason :: Lens.Lens' StartReplayResponse (Prelude.Maybe Prelude.Text)
startReplayResponse_stateReason = Lens.lens (\StartReplayResponse' {stateReason} -> stateReason) (\s@StartReplayResponse' {} a -> s {stateReason = a} :: StartReplayResponse)

-- | The response's http status code.
startReplayResponse_httpStatus :: Lens.Lens' StartReplayResponse Prelude.Int
startReplayResponse_httpStatus = Lens.lens (\StartReplayResponse' {httpStatus} -> httpStatus) (\s@StartReplayResponse' {} a -> s {httpStatus = a} :: StartReplayResponse)

instance Prelude.NFData StartReplayResponse where
  rnf StartReplayResponse' {..} =
    Prelude.rnf replayArn `Prelude.seq`
      Prelude.rnf replayStartTime `Prelude.seq`
        Prelude.rnf state `Prelude.seq`
          Prelude.rnf stateReason `Prelude.seq`
            Prelude.rnf httpStatus
