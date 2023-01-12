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
-- Module      : Amazonka.Nimble.Types.StreamingSessionBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionBackup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StreamingSessionState
import Amazonka.Nimble.Types.StreamingSessionStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Information about the streaming session backup.
--
-- /See:/ 'newStreamingSessionBackup' smart constructor.
data StreamingSessionBackup = StreamingSessionBackup'
  { -- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
    -- uniquely identifies it. ARNs are unique across all Regions.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the backup.
    backupId :: Prelude.Maybe Prelude.Text,
    -- | The ISO timestamp in for when the resource was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the launch profile which allowed the backups for the streaming
    -- session.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that owns the streaming session.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The streaming session ID for the @StreamingSessionBackup@.
    sessionId :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe StreamingSessionState,
    -- | The status code.
    statusCode :: Prelude.Maybe StreamingSessionStatusCode,
    -- | The status message for the streaming session backup.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key-value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingSessionBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'streamingSessionBackup_arn' - The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
--
-- 'backupId', 'streamingSessionBackup_backupId' - The ID of the backup.
--
-- 'createdAt', 'streamingSessionBackup_createdAt' - The ISO timestamp in for when the resource was created.
--
-- 'launchProfileId', 'streamingSessionBackup_launchProfileId' - The ID of the launch profile which allowed the backups for the streaming
-- session.
--
-- 'ownedBy', 'streamingSessionBackup_ownedBy' - The user ID of the user that owns the streaming session.
--
-- 'sessionId', 'streamingSessionBackup_sessionId' - The streaming session ID for the @StreamingSessionBackup@.
--
-- 'state', 'streamingSessionBackup_state' - Undocumented member.
--
-- 'statusCode', 'streamingSessionBackup_statusCode' - The status code.
--
-- 'statusMessage', 'streamingSessionBackup_statusMessage' - The status message for the streaming session backup.
--
-- 'tags', 'streamingSessionBackup_tags' - A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
newStreamingSessionBackup ::
  StreamingSessionBackup
newStreamingSessionBackup =
  StreamingSessionBackup'
    { arn = Prelude.Nothing,
      backupId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      state = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
streamingSessionBackup_arn :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_arn = Lens.lens (\StreamingSessionBackup' {arn} -> arn) (\s@StreamingSessionBackup' {} a -> s {arn = a} :: StreamingSessionBackup)

-- | The ID of the backup.
streamingSessionBackup_backupId :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_backupId = Lens.lens (\StreamingSessionBackup' {backupId} -> backupId) (\s@StreamingSessionBackup' {} a -> s {backupId = a} :: StreamingSessionBackup)

-- | The ISO timestamp in for when the resource was created.
streamingSessionBackup_createdAt :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.UTCTime)
streamingSessionBackup_createdAt = Lens.lens (\StreamingSessionBackup' {createdAt} -> createdAt) (\s@StreamingSessionBackup' {} a -> s {createdAt = a} :: StreamingSessionBackup) Prelude.. Lens.mapping Data._Time

-- | The ID of the launch profile which allowed the backups for the streaming
-- session.
streamingSessionBackup_launchProfileId :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_launchProfileId = Lens.lens (\StreamingSessionBackup' {launchProfileId} -> launchProfileId) (\s@StreamingSessionBackup' {} a -> s {launchProfileId = a} :: StreamingSessionBackup)

-- | The user ID of the user that owns the streaming session.
streamingSessionBackup_ownedBy :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_ownedBy = Lens.lens (\StreamingSessionBackup' {ownedBy} -> ownedBy) (\s@StreamingSessionBackup' {} a -> s {ownedBy = a} :: StreamingSessionBackup)

-- | The streaming session ID for the @StreamingSessionBackup@.
streamingSessionBackup_sessionId :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_sessionId = Lens.lens (\StreamingSessionBackup' {sessionId} -> sessionId) (\s@StreamingSessionBackup' {} a -> s {sessionId = a} :: StreamingSessionBackup)

-- | Undocumented member.
streamingSessionBackup_state :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe StreamingSessionState)
streamingSessionBackup_state = Lens.lens (\StreamingSessionBackup' {state} -> state) (\s@StreamingSessionBackup' {} a -> s {state = a} :: StreamingSessionBackup)

-- | The status code.
streamingSessionBackup_statusCode :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe StreamingSessionStatusCode)
streamingSessionBackup_statusCode = Lens.lens (\StreamingSessionBackup' {statusCode} -> statusCode) (\s@StreamingSessionBackup' {} a -> s {statusCode = a} :: StreamingSessionBackup)

-- | The status message for the streaming session backup.
streamingSessionBackup_statusMessage :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe Prelude.Text)
streamingSessionBackup_statusMessage = Lens.lens (\StreamingSessionBackup' {statusMessage} -> statusMessage) (\s@StreamingSessionBackup' {} a -> s {statusMessage = a} :: StreamingSessionBackup)

-- | A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
streamingSessionBackup_tags :: Lens.Lens' StreamingSessionBackup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamingSessionBackup_tags = Lens.lens (\StreamingSessionBackup' {tags} -> tags) (\s@StreamingSessionBackup' {} a -> s {tags = a} :: StreamingSessionBackup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StreamingSessionBackup where
  parseJSON =
    Data.withObject
      "StreamingSessionBackup"
      ( \x ->
          StreamingSessionBackup'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "backupId")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "launchProfileId")
            Prelude.<*> (x Data..:? "ownedBy")
            Prelude.<*> (x Data..:? "sessionId")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StreamingSessionBackup where
  hashWithSalt _salt StreamingSessionBackup' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags

instance Prelude.NFData StreamingSessionBackup where
  rnf StreamingSessionBackup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
