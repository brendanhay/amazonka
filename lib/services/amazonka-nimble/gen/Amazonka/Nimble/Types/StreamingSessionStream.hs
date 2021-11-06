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
-- Module      : Amazonka.Nimble.Types.StreamingSessionStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StreamingSessionStreamState
import Amazonka.Nimble.Types.StreamingSessionStreamStatusCode
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newStreamingSessionStream' smart constructor.
data StreamingSessionStream = StreamingSessionStream'
  { -- | The user ID of the user that owns the streaming session.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe StreamingSessionStreamState,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource expires.
    expiresAt :: Prelude.Maybe Core.POSIX,
    -- | The URL to connect to this stream using the DCV client.
    url :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that created the streaming session stream.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The streaming session stream status code.
    statusCode :: Prelude.Maybe StreamingSessionStreamStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingSessionStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownedBy', 'streamingSessionStream_ownedBy' - The user ID of the user that owns the streaming session.
--
-- 'state', 'streamingSessionStream_state' - The current state.
--
-- 'createdAt', 'streamingSessionStream_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'expiresAt', 'streamingSessionStream_expiresAt' - The Unix epoch timestamp in seconds for when the resource expires.
--
-- 'url', 'streamingSessionStream_url' - The URL to connect to this stream using the DCV client.
--
-- 'createdBy', 'streamingSessionStream_createdBy' - The user ID of the user that created the streaming session stream.
--
-- 'streamId', 'streamingSessionStream_streamId' - The stream ID.
--
-- 'statusCode', 'streamingSessionStream_statusCode' - The streaming session stream status code.
newStreamingSessionStream ::
  StreamingSessionStream
newStreamingSessionStream =
  StreamingSessionStream'
    { ownedBy = Prelude.Nothing,
      state = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      expiresAt = Prelude.Nothing,
      url = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      streamId = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The user ID of the user that owns the streaming session.
streamingSessionStream_ownedBy :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.Text)
streamingSessionStream_ownedBy = Lens.lens (\StreamingSessionStream' {ownedBy} -> ownedBy) (\s@StreamingSessionStream' {} a -> s {ownedBy = a} :: StreamingSessionStream)

-- | The current state.
streamingSessionStream_state :: Lens.Lens' StreamingSessionStream (Prelude.Maybe StreamingSessionStreamState)
streamingSessionStream_state = Lens.lens (\StreamingSessionStream' {state} -> state) (\s@StreamingSessionStream' {} a -> s {state = a} :: StreamingSessionStream)

-- | The Unix epoch timestamp in seconds for when the resource was created.
streamingSessionStream_createdAt :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.UTCTime)
streamingSessionStream_createdAt = Lens.lens (\StreamingSessionStream' {createdAt} -> createdAt) (\s@StreamingSessionStream' {} a -> s {createdAt = a} :: StreamingSessionStream) Prelude.. Lens.mapping Core._Time

-- | The Unix epoch timestamp in seconds for when the resource expires.
streamingSessionStream_expiresAt :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.UTCTime)
streamingSessionStream_expiresAt = Lens.lens (\StreamingSessionStream' {expiresAt} -> expiresAt) (\s@StreamingSessionStream' {} a -> s {expiresAt = a} :: StreamingSessionStream) Prelude.. Lens.mapping Core._Time

-- | The URL to connect to this stream using the DCV client.
streamingSessionStream_url :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.Text)
streamingSessionStream_url = Lens.lens (\StreamingSessionStream' {url} -> url) (\s@StreamingSessionStream' {} a -> s {url = a} :: StreamingSessionStream)

-- | The user ID of the user that created the streaming session stream.
streamingSessionStream_createdBy :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.Text)
streamingSessionStream_createdBy = Lens.lens (\StreamingSessionStream' {createdBy} -> createdBy) (\s@StreamingSessionStream' {} a -> s {createdBy = a} :: StreamingSessionStream)

-- | The stream ID.
streamingSessionStream_streamId :: Lens.Lens' StreamingSessionStream (Prelude.Maybe Prelude.Text)
streamingSessionStream_streamId = Lens.lens (\StreamingSessionStream' {streamId} -> streamId) (\s@StreamingSessionStream' {} a -> s {streamId = a} :: StreamingSessionStream)

-- | The streaming session stream status code.
streamingSessionStream_statusCode :: Lens.Lens' StreamingSessionStream (Prelude.Maybe StreamingSessionStreamStatusCode)
streamingSessionStream_statusCode = Lens.lens (\StreamingSessionStream' {statusCode} -> statusCode) (\s@StreamingSessionStream' {} a -> s {statusCode = a} :: StreamingSessionStream)

instance Core.FromJSON StreamingSessionStream where
  parseJSON =
    Core.withObject
      "StreamingSessionStream"
      ( \x ->
          StreamingSessionStream'
            Prelude.<$> (x Core..:? "ownedBy")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "expiresAt")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "statusCode")
      )

instance Prelude.Hashable StreamingSessionStream

instance Prelude.NFData StreamingSessionStream
