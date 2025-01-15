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
-- Module      : Amazonka.IVS.Types.Stream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.Stream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.StreamHealth
import Amazonka.IVS.Types.StreamState
import qualified Amazonka.Prelude as Prelude

-- | Specifies a live video stream that has been ingested and distributed.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | Channel ARN for the stream.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The stream’s health.
    health :: Prelude.Maybe StreamHealth,
    -- | URL of the master playlist, required by the video player to play the HLS
    -- stream.
    playbackUrl :: Prelude.Maybe Prelude.Text,
    -- | Time of the stream’s start. This is an ISO 8601 timestamp; /note that
    -- this is returned as a string/.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The stream’s state.
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
-- Create a value of 'Stream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'stream_channelArn' - Channel ARN for the stream.
--
-- 'health', 'stream_health' - The stream’s health.
--
-- 'playbackUrl', 'stream_playbackUrl' - URL of the master playlist, required by the video player to play the HLS
-- stream.
--
-- 'startTime', 'stream_startTime' - Time of the stream’s start. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
--
-- 'state', 'stream_state' - The stream’s state.
--
-- 'streamId', 'stream_streamId' - Unique identifier for a live or previously live stream in the specified
-- channel.
--
-- 'viewerCount', 'stream_viewerCount' - A count of concurrent views of the stream. Typically, a new view appears
-- in @viewerCount@ within 15 seconds of when video playback starts and a
-- view is removed from @viewerCount@ within 1 minute of when video
-- playback ends. A value of -1 indicates that the request timed out; in
-- this case, retry.
newStream ::
  Stream
newStream =
  Stream'
    { channelArn = Prelude.Nothing,
      health = Prelude.Nothing,
      playbackUrl = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      streamId = Prelude.Nothing,
      viewerCount = Prelude.Nothing
    }

-- | Channel ARN for the stream.
stream_channelArn :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_channelArn = Lens.lens (\Stream' {channelArn} -> channelArn) (\s@Stream' {} a -> s {channelArn = a} :: Stream)

-- | The stream’s health.
stream_health :: Lens.Lens' Stream (Prelude.Maybe StreamHealth)
stream_health = Lens.lens (\Stream' {health} -> health) (\s@Stream' {} a -> s {health = a} :: Stream)

-- | URL of the master playlist, required by the video player to play the HLS
-- stream.
stream_playbackUrl :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_playbackUrl = Lens.lens (\Stream' {playbackUrl} -> playbackUrl) (\s@Stream' {} a -> s {playbackUrl = a} :: Stream)

-- | Time of the stream’s start. This is an ISO 8601 timestamp; /note that
-- this is returned as a string/.
stream_startTime :: Lens.Lens' Stream (Prelude.Maybe Prelude.UTCTime)
stream_startTime = Lens.lens (\Stream' {startTime} -> startTime) (\s@Stream' {} a -> s {startTime = a} :: Stream) Prelude.. Lens.mapping Data._Time

-- | The stream’s state.
stream_state :: Lens.Lens' Stream (Prelude.Maybe StreamState)
stream_state = Lens.lens (\Stream' {state} -> state) (\s@Stream' {} a -> s {state = a} :: Stream)

-- | Unique identifier for a live or previously live stream in the specified
-- channel.
stream_streamId :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamId = Lens.lens (\Stream' {streamId} -> streamId) (\s@Stream' {} a -> s {streamId = a} :: Stream)

-- | A count of concurrent views of the stream. Typically, a new view appears
-- in @viewerCount@ within 15 seconds of when video playback starts and a
-- view is removed from @viewerCount@ within 1 minute of when video
-- playback ends. A value of -1 indicates that the request timed out; in
-- this case, retry.
stream_viewerCount :: Lens.Lens' Stream (Prelude.Maybe Prelude.Integer)
stream_viewerCount = Lens.lens (\Stream' {viewerCount} -> viewerCount) (\s@Stream' {} a -> s {viewerCount = a} :: Stream)

instance Data.FromJSON Stream where
  parseJSON =
    Data.withObject
      "Stream"
      ( \x ->
          Stream'
            Prelude.<$> (x Data..:? "channelArn")
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "playbackUrl")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "streamId")
            Prelude.<*> (x Data..:? "viewerCount")
      )

instance Prelude.Hashable Stream where
  hashWithSalt _salt Stream' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` playbackUrl
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` viewerCount

instance Prelude.NFData Stream where
  rnf Stream' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf health `Prelude.seq`
        Prelude.rnf playbackUrl `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf streamId `Prelude.seq`
                Prelude.rnf viewerCount
