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
-- Module      : Amazonka.IVS.Types.StreamSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.Channel
import Amazonka.IVS.Types.IngestConfiguration
import Amazonka.IVS.Types.RecordingConfiguration
import Amazonka.IVS.Types.StreamEvent
import qualified Amazonka.Prelude as Prelude

-- | Object that captures the Amazon IVS configuration that the customer
-- provisioned, the ingest configurations that the broadcaster used, and
-- the most recent Amazon IVS stream events it encountered.
--
-- /See:/ 'newStreamSession' smart constructor.
data StreamSession = StreamSession'
  { -- | The properties of the channel at the time of going live.
    channel :: Prelude.Maybe Channel,
    -- | Time when the channel went offline. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/. For live streams, this is @NULL@.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The properties of the incoming RTMP stream for the stream.
    ingestConfiguration :: Prelude.Maybe IngestConfiguration,
    -- | The properties of recording the live stream.
    recordingConfiguration :: Prelude.Maybe RecordingConfiguration,
    -- | Time when the channel went live. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | Unique identifier for a live or previously live stream in the specified
    -- channel.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | List of Amazon IVS events that the stream encountered. The list is
    -- sorted by most recent events and contains up to 500 events. For Amazon
    -- IVS events, see
    -- <https://docs.aws.amazon.com/ivs/latest/userguide/eventbridge.html Using Amazon EventBridge with Amazon IVS>.
    truncatedEvents :: Prelude.Maybe [StreamEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'streamSession_channel' - The properties of the channel at the time of going live.
--
-- 'endTime', 'streamSession_endTime' - Time when the channel went offline. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/. For live streams, this is @NULL@.
--
-- 'ingestConfiguration', 'streamSession_ingestConfiguration' - The properties of the incoming RTMP stream for the stream.
--
-- 'recordingConfiguration', 'streamSession_recordingConfiguration' - The properties of recording the live stream.
--
-- 'startTime', 'streamSession_startTime' - Time when the channel went live. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'streamId', 'streamSession_streamId' - Unique identifier for a live or previously live stream in the specified
-- channel.
--
-- 'truncatedEvents', 'streamSession_truncatedEvents' - List of Amazon IVS events that the stream encountered. The list is
-- sorted by most recent events and contains up to 500 events. For Amazon
-- IVS events, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/eventbridge.html Using Amazon EventBridge with Amazon IVS>.
newStreamSession ::
  StreamSession
newStreamSession =
  StreamSession'
    { channel = Prelude.Nothing,
      endTime = Prelude.Nothing,
      ingestConfiguration = Prelude.Nothing,
      recordingConfiguration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      streamId = Prelude.Nothing,
      truncatedEvents = Prelude.Nothing
    }

-- | The properties of the channel at the time of going live.
streamSession_channel :: Lens.Lens' StreamSession (Prelude.Maybe Channel)
streamSession_channel = Lens.lens (\StreamSession' {channel} -> channel) (\s@StreamSession' {} a -> s {channel = a} :: StreamSession)

-- | Time when the channel went offline. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/. For live streams, this is @NULL@.
streamSession_endTime :: Lens.Lens' StreamSession (Prelude.Maybe Prelude.UTCTime)
streamSession_endTime = Lens.lens (\StreamSession' {endTime} -> endTime) (\s@StreamSession' {} a -> s {endTime = a} :: StreamSession) Prelude.. Lens.mapping Data._Time

-- | The properties of the incoming RTMP stream for the stream.
streamSession_ingestConfiguration :: Lens.Lens' StreamSession (Prelude.Maybe IngestConfiguration)
streamSession_ingestConfiguration = Lens.lens (\StreamSession' {ingestConfiguration} -> ingestConfiguration) (\s@StreamSession' {} a -> s {ingestConfiguration = a} :: StreamSession)

-- | The properties of recording the live stream.
streamSession_recordingConfiguration :: Lens.Lens' StreamSession (Prelude.Maybe RecordingConfiguration)
streamSession_recordingConfiguration = Lens.lens (\StreamSession' {recordingConfiguration} -> recordingConfiguration) (\s@StreamSession' {} a -> s {recordingConfiguration = a} :: StreamSession)

-- | Time when the channel went live. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
streamSession_startTime :: Lens.Lens' StreamSession (Prelude.Maybe Prelude.UTCTime)
streamSession_startTime = Lens.lens (\StreamSession' {startTime} -> startTime) (\s@StreamSession' {} a -> s {startTime = a} :: StreamSession) Prelude.. Lens.mapping Data._Time

-- | Unique identifier for a live or previously live stream in the specified
-- channel.
streamSession_streamId :: Lens.Lens' StreamSession (Prelude.Maybe Prelude.Text)
streamSession_streamId = Lens.lens (\StreamSession' {streamId} -> streamId) (\s@StreamSession' {} a -> s {streamId = a} :: StreamSession)

-- | List of Amazon IVS events that the stream encountered. The list is
-- sorted by most recent events and contains up to 500 events. For Amazon
-- IVS events, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/eventbridge.html Using Amazon EventBridge with Amazon IVS>.
streamSession_truncatedEvents :: Lens.Lens' StreamSession (Prelude.Maybe [StreamEvent])
streamSession_truncatedEvents = Lens.lens (\StreamSession' {truncatedEvents} -> truncatedEvents) (\s@StreamSession' {} a -> s {truncatedEvents = a} :: StreamSession) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StreamSession where
  parseJSON =
    Data.withObject
      "StreamSession"
      ( \x ->
          StreamSession'
            Prelude.<$> (x Data..:? "channel")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "ingestConfiguration")
            Prelude.<*> (x Data..:? "recordingConfiguration")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "streamId")
            Prelude.<*> ( x
                            Data..:? "truncatedEvents"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable StreamSession where
  hashWithSalt _salt StreamSession' {..} =
    _salt
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` ingestConfiguration
      `Prelude.hashWithSalt` recordingConfiguration
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` truncatedEvents

instance Prelude.NFData StreamSession where
  rnf StreamSession' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf ingestConfiguration
      `Prelude.seq` Prelude.rnf recordingConfiguration
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf truncatedEvents
