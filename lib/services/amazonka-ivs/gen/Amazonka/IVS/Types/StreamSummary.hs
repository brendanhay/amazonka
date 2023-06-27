{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVS.Types.StreamSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.StreamHealth
import Amazonka.IVS.Types.StreamState
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a stream.
--
-- /See:/ 'newStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | Channel ARN for the stream.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The stream’s health.
    health :: Prelude.Maybe StreamHealth,
    -- | Time of the stream’s start. This is an ISO 8601 timestamp; /note that
    -- this is returned as a string/.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The stream’s state. Do not rely on the @OFFLINE@ state, as the API may
    -- not return it; instead, a \"NotBroadcasting\" error will indicate that
    -- the stream is not live.
    state :: Prelude.Maybe StreamState,
    -- | Unique identifier for a live or previously live stream in the specified
    -- channel.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | A count of concurrent views of the stream. Typically, a new view appears
    -- in @viewerCount@ within 15 seconds of when video playback starts and a
    -- view is removed from @viewerCount@ within 1 minute of when video
    -- playback ends. A value of -1 indicates that the request timed out; in
    -- this case, retry.
    viewerCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'streamSummary_channelArn' - Channel ARN for the stream.
--
-- 'health', 'streamSummary_health' - The stream’s health.
--
-- 'startTime', 'streamSummary_startTime' - Time of the stream’s start. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
--
-- 'state', 'streamSummary_state' - The stream’s state. Do not rely on the @OFFLINE@ state, as the API may
-- not return it; instead, a \"NotBroadcasting\" error will indicate that
-- the stream is not live.
--
-- 'streamId', 'streamSummary_streamId' - Unique identifier for a live or previously live stream in the specified
-- channel.
--
-- 'viewerCount', 'streamSummary_viewerCount' - A count of concurrent views of the stream. Typically, a new view appears
-- in @viewerCount@ within 15 seconds of when video playback starts and a
-- view is removed from @viewerCount@ within 1 minute of when video
-- playback ends. A value of -1 indicates that the request timed out; in
-- this case, retry.
newStreamSummary ::
  StreamSummary
newStreamSummary =
  StreamSummary'
    { channelArn = Prelude.Nothing,
      health = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      streamId = Prelude.Nothing,
      viewerCount = Prelude.Nothing
    }

-- | Channel ARN for the stream.
streamSummary_channelArn :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_channelArn = Lens.lens (\StreamSummary' {channelArn} -> channelArn) (\s@StreamSummary' {} a -> s {channelArn = a} :: StreamSummary)

-- | The stream’s health.
streamSummary_health :: Lens.Lens' StreamSummary (Prelude.Maybe StreamHealth)
streamSummary_health = Lens.lens (\StreamSummary' {health} -> health) (\s@StreamSummary' {} a -> s {health = a} :: StreamSummary)

-- | Time of the stream’s start. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
streamSummary_startTime :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.UTCTime)
streamSummary_startTime = Lens.lens (\StreamSummary' {startTime} -> startTime) (\s@StreamSummary' {} a -> s {startTime = a} :: StreamSummary) Prelude.. Lens.mapping Data._Time

-- | The stream’s state. Do not rely on the @OFFLINE@ state, as the API may
-- not return it; instead, a \"NotBroadcasting\" error will indicate that
-- the stream is not live.
streamSummary_state :: Lens.Lens' StreamSummary (Prelude.Maybe StreamState)
streamSummary_state = Lens.lens (\StreamSummary' {state} -> state) (\s@StreamSummary' {} a -> s {state = a} :: StreamSummary)

-- | Unique identifier for a live or previously live stream in the specified
-- channel.
streamSummary_streamId :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Text)
streamSummary_streamId = Lens.lens (\StreamSummary' {streamId} -> streamId) (\s@StreamSummary' {} a -> s {streamId = a} :: StreamSummary)

-- | A count of concurrent views of the stream. Typically, a new view appears
-- in @viewerCount@ within 15 seconds of when video playback starts and a
-- view is removed from @viewerCount@ within 1 minute of when video
-- playback ends. A value of -1 indicates that the request timed out; in
-- this case, retry.
streamSummary_viewerCount :: Lens.Lens' StreamSummary (Prelude.Maybe Prelude.Integer)
streamSummary_viewerCount = Lens.lens (\StreamSummary' {viewerCount} -> viewerCount) (\s@StreamSummary' {} a -> s {viewerCount = a} :: StreamSummary)

instance Data.FromJSON StreamSummary where
  parseJSON =
    Data.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Prelude.<$> (x Data..:? "channelArn")
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "streamId")
            Prelude.<*> (x Data..:? "viewerCount")
      )

instance Prelude.Hashable StreamSummary where
  hashWithSalt _salt StreamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` viewerCount

instance Prelude.NFData StreamSummary where
  rnf StreamSummary' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf viewerCount
