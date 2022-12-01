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
-- Module      : Amazonka.Nimble.Types.StreamingSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.StreamingSessionState
import Amazonka.Nimble.Types.StreamingSessionStatusCode
import qualified Amazonka.Prelude as Prelude

-- | A streaming session is a virtual workstation created using a particular
-- launch profile.
--
-- /See:/ 'newStreamingSession' smart constructor.
data StreamingSession = StreamingSession'
  { -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the streaming image.
    streamingImageId :: Prelude.Maybe Prelude.Text,
    -- | The time the streaming session will automatically be stopped if the user
    -- doesn’t stop the session themselves.
    stopAt :: Prelude.Maybe Core.POSIX,
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | The EC2 Instance type used for the streaming session.
    ec2InstanceType :: Prelude.Maybe Prelude.Text,
    -- | The time the streaming session will automatically terminate if not
    -- terminated by the user.
    terminateAt :: Prelude.Maybe Core.POSIX,
    -- | The user ID of the user that stopped the streaming session.
    stoppedBy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe StreamingSessionState,
    -- | The user ID of the user that started the streaming session.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The time the session entered START_IN_PROGRESS state.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The time the session entered STOP_IN_PROGRESS state.
    stoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The status code.
    statusCode :: Prelude.Maybe StreamingSessionStatusCode,
    -- | The user ID of the user that created the streaming session.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The status message for the streaming session.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that owns the streaming session. The user that
    -- owns the session will be logging into the session and interacting with
    -- the virtual workstation.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'streamingSession_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'launchProfileId', 'streamingSession_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'streamingImageId', 'streamingSession_streamingImageId' - The ID of the streaming image.
--
-- 'stopAt', 'streamingSession_stopAt' - The time the streaming session will automatically be stopped if the user
-- doesn’t stop the session themselves.
--
-- 'updatedBy', 'streamingSession_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'ec2InstanceType', 'streamingSession_ec2InstanceType' - The EC2 Instance type used for the streaming session.
--
-- 'terminateAt', 'streamingSession_terminateAt' - The time the streaming session will automatically terminate if not
-- terminated by the user.
--
-- 'stoppedBy', 'streamingSession_stoppedBy' - The user ID of the user that stopped the streaming session.
--
-- 'arn', 'streamingSession_arn' - The ARN of the resource.
--
-- 'state', 'streamingSession_state' - The current state.
--
-- 'startedBy', 'streamingSession_startedBy' - The user ID of the user that started the streaming session.
--
-- 'startedAt', 'streamingSession_startedAt' - The time the session entered START_IN_PROGRESS state.
--
-- 'sessionId', 'streamingSession_sessionId' - The session ID.
--
-- 'stoppedAt', 'streamingSession_stoppedAt' - The time the session entered STOP_IN_PROGRESS state.
--
-- 'statusCode', 'streamingSession_statusCode' - The status code.
--
-- 'createdBy', 'streamingSession_createdBy' - The user ID of the user that created the streaming session.
--
-- 'statusMessage', 'streamingSession_statusMessage' - The status message for the streaming session.
--
-- 'ownedBy', 'streamingSession_ownedBy' - The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
--
-- 'createdAt', 'streamingSession_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'updatedAt', 'streamingSession_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
newStreamingSession ::
  StreamingSession
newStreamingSession =
  StreamingSession'
    { tags = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      streamingImageId = Prelude.Nothing,
      stopAt = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      ec2InstanceType = Prelude.Nothing,
      terminateAt = Prelude.Nothing,
      stoppedBy = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      stoppedAt = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
streamingSession_tags :: Lens.Lens' StreamingSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamingSession_tags = Lens.lens (\StreamingSession' {tags} -> tags) (\s@StreamingSession' {} a -> s {tags = a} :: StreamingSession) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the launch profile used to control access from the streaming
-- session.
streamingSession_launchProfileId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_launchProfileId = Lens.lens (\StreamingSession' {launchProfileId} -> launchProfileId) (\s@StreamingSession' {} a -> s {launchProfileId = a} :: StreamingSession)

-- | The ID of the streaming image.
streamingSession_streamingImageId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_streamingImageId = Lens.lens (\StreamingSession' {streamingImageId} -> streamingImageId) (\s@StreamingSession' {} a -> s {streamingImageId = a} :: StreamingSession)

-- | The time the streaming session will automatically be stopped if the user
-- doesn’t stop the session themselves.
streamingSession_stopAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_stopAt = Lens.lens (\StreamingSession' {stopAt} -> stopAt) (\s@StreamingSession' {} a -> s {stopAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

-- | The user ID of the user that most recently updated the resource.
streamingSession_updatedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_updatedBy = Lens.lens (\StreamingSession' {updatedBy} -> updatedBy) (\s@StreamingSession' {} a -> s {updatedBy = a} :: StreamingSession)

-- | The EC2 Instance type used for the streaming session.
streamingSession_ec2InstanceType :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_ec2InstanceType = Lens.lens (\StreamingSession' {ec2InstanceType} -> ec2InstanceType) (\s@StreamingSession' {} a -> s {ec2InstanceType = a} :: StreamingSession)

-- | The time the streaming session will automatically terminate if not
-- terminated by the user.
streamingSession_terminateAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_terminateAt = Lens.lens (\StreamingSession' {terminateAt} -> terminateAt) (\s@StreamingSession' {} a -> s {terminateAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

-- | The user ID of the user that stopped the streaming session.
streamingSession_stoppedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_stoppedBy = Lens.lens (\StreamingSession' {stoppedBy} -> stoppedBy) (\s@StreamingSession' {} a -> s {stoppedBy = a} :: StreamingSession)

-- | The ARN of the resource.
streamingSession_arn :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_arn = Lens.lens (\StreamingSession' {arn} -> arn) (\s@StreamingSession' {} a -> s {arn = a} :: StreamingSession)

-- | The current state.
streamingSession_state :: Lens.Lens' StreamingSession (Prelude.Maybe StreamingSessionState)
streamingSession_state = Lens.lens (\StreamingSession' {state} -> state) (\s@StreamingSession' {} a -> s {state = a} :: StreamingSession)

-- | The user ID of the user that started the streaming session.
streamingSession_startedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_startedBy = Lens.lens (\StreamingSession' {startedBy} -> startedBy) (\s@StreamingSession' {} a -> s {startedBy = a} :: StreamingSession)

-- | The time the session entered START_IN_PROGRESS state.
streamingSession_startedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_startedAt = Lens.lens (\StreamingSession' {startedAt} -> startedAt) (\s@StreamingSession' {} a -> s {startedAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

-- | The session ID.
streamingSession_sessionId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_sessionId = Lens.lens (\StreamingSession' {sessionId} -> sessionId) (\s@StreamingSession' {} a -> s {sessionId = a} :: StreamingSession)

-- | The time the session entered STOP_IN_PROGRESS state.
streamingSession_stoppedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_stoppedAt = Lens.lens (\StreamingSession' {stoppedAt} -> stoppedAt) (\s@StreamingSession' {} a -> s {stoppedAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

-- | The status code.
streamingSession_statusCode :: Lens.Lens' StreamingSession (Prelude.Maybe StreamingSessionStatusCode)
streamingSession_statusCode = Lens.lens (\StreamingSession' {statusCode} -> statusCode) (\s@StreamingSession' {} a -> s {statusCode = a} :: StreamingSession)

-- | The user ID of the user that created the streaming session.
streamingSession_createdBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_createdBy = Lens.lens (\StreamingSession' {createdBy} -> createdBy) (\s@StreamingSession' {} a -> s {createdBy = a} :: StreamingSession)

-- | The status message for the streaming session.
streamingSession_statusMessage :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_statusMessage = Lens.lens (\StreamingSession' {statusMessage} -> statusMessage) (\s@StreamingSession' {} a -> s {statusMessage = a} :: StreamingSession)

-- | The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
streamingSession_ownedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_ownedBy = Lens.lens (\StreamingSession' {ownedBy} -> ownedBy) (\s@StreamingSession' {} a -> s {ownedBy = a} :: StreamingSession)

-- | The Unix epoch timestamp in seconds for when the resource was created.
streamingSession_createdAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_createdAt = Lens.lens (\StreamingSession' {createdAt} -> createdAt) (\s@StreamingSession' {} a -> s {createdAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was updated.
streamingSession_updatedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_updatedAt = Lens.lens (\StreamingSession' {updatedAt} -> updatedAt) (\s@StreamingSession' {} a -> s {updatedAt = a} :: StreamingSession) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON StreamingSession where
  parseJSON =
    Core.withObject
      "StreamingSession"
      ( \x ->
          StreamingSession'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "launchProfileId")
            Prelude.<*> (x Core..:? "streamingImageId")
            Prelude.<*> (x Core..:? "stopAt")
            Prelude.<*> (x Core..:? "updatedBy")
            Prelude.<*> (x Core..:? "ec2InstanceType")
            Prelude.<*> (x Core..:? "terminateAt")
            Prelude.<*> (x Core..:? "stoppedBy")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "startedBy")
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> (x Core..:? "sessionId")
            Prelude.<*> (x Core..:? "stoppedAt")
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "ownedBy")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable StreamingSession where
  hashWithSalt _salt StreamingSession' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` stopAt
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` ec2InstanceType
      `Prelude.hashWithSalt` terminateAt
      `Prelude.hashWithSalt` stoppedBy
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` stoppedAt
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData StreamingSession where
  rnf StreamingSession' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf stopAt
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf ec2InstanceType
      `Prelude.seq` Prelude.rnf terminateAt
      `Prelude.seq` Prelude.rnf stoppedBy
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf stoppedAt
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
