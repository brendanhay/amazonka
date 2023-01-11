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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.AutomaticTerminationMode
import Amazonka.Nimble.Types.SessionBackupMode
import Amazonka.Nimble.Types.SessionPersistenceMode
import Amazonka.Nimble.Types.StreamingSessionState
import Amazonka.Nimble.Types.StreamingSessionStatusCode
import Amazonka.Nimble.Types.VolumeConfiguration
import Amazonka.Nimble.Types.VolumeRetentionMode
import qualified Amazonka.Prelude as Prelude

-- | A streaming session is a virtual workstation created using a particular
-- launch profile.
--
-- /See:/ 'newStreamingSession' smart constructor.
data StreamingSession = StreamingSession'
  { -- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
    -- uniquely identifies it. ARNs are unique across all Regions.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates if a streaming session created from this launch profile should
    -- be terminated automatically or retained without termination after being
    -- in a @STOPPED@ state.
    --
    -- -   When @ACTIVATED@, the streaming session is scheduled for termination
    --     after being in the @STOPPED@ state for the time specified in
    --     @maxStoppedSessionLengthInMinutes@.
    --
    -- -   When @DEACTIVATED@, the streaming session can remain in the
    --     @STOPPED@ state indefinitely.
    --
    -- This parameter is only allowed when @sessionPersistenceMode@ is
    -- @ACTIVATED@. When allowed, the default value for this parameter is
    -- @DEACTIVATED@.
    automaticTerminationMode :: Prelude.Maybe AutomaticTerminationMode,
    -- | Shows the current backup setting of the session.
    backupMode :: Prelude.Maybe SessionBackupMode,
    -- | The ISO timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The user ID of the user that created the streaming session.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The EC2 Instance type used for the streaming session.
    ec2InstanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of backups of a streaming session that you can have.
    -- When the maximum number of backups is reached, the oldest backup is
    -- deleted.
    maxBackupsToRetain :: Prelude.Maybe Prelude.Natural,
    -- | The user ID of the user that owns the streaming session. The user that
    -- owns the session will be logging into the session and interacting with
    -- the virtual workstation.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Determine if a streaming session created from this launch profile can
    -- configure persistent storage. This means that @volumeConfiguration@ and
    -- @automaticTerminationMode@ are configured.
    sessionPersistenceMode :: Prelude.Maybe SessionPersistenceMode,
    -- | The time the session entered @START_IN_PROGRESS@ state.
    startedAt :: Prelude.Maybe Data.ISO8601,
    -- | The user ID of the user that started the streaming session.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The backup ID used to restore a streaming session.
    startedFromBackupId :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe StreamingSessionState,
    -- | The status code.
    statusCode :: Prelude.Maybe StreamingSessionStatusCode,
    -- | The status message for the streaming session.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the streaming session will automatically be stopped if the user
    -- doesn’t stop the session themselves.
    stopAt :: Prelude.Maybe Data.ISO8601,
    -- | The time the session entered @STOP_IN_PROGRESS@ state.
    stoppedAt :: Prelude.Maybe Data.ISO8601,
    -- | The user ID of the user that stopped the streaming session.
    stoppedBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the streaming image.
    streamingImageId :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key-value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time the streaming session will automatically terminate if not
    -- terminated by the user.
    terminateAt :: Prelude.Maybe Data.ISO8601,
    -- | The ISO timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | Custom volume configuration for the root volumes that are attached to
    -- streaming sessions.
    --
    -- This parameter is only allowed when @sessionPersistenceMode@ is
    -- @ACTIVATED@.
    volumeConfiguration :: Prelude.Maybe VolumeConfiguration,
    -- | Determine if an EBS volume created from this streaming session will be
    -- backed up.
    volumeRetentionMode :: Prelude.Maybe VolumeRetentionMode
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
-- 'arn', 'streamingSession_arn' - The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
--
-- 'automaticTerminationMode', 'streamingSession_automaticTerminationMode' - Indicates if a streaming session created from this launch profile should
-- be terminated automatically or retained without termination after being
-- in a @STOPPED@ state.
--
-- -   When @ACTIVATED@, the streaming session is scheduled for termination
--     after being in the @STOPPED@ state for the time specified in
--     @maxStoppedSessionLengthInMinutes@.
--
-- -   When @DEACTIVATED@, the streaming session can remain in the
--     @STOPPED@ state indefinitely.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@. When allowed, the default value for this parameter is
-- @DEACTIVATED@.
--
-- 'backupMode', 'streamingSession_backupMode' - Shows the current backup setting of the session.
--
-- 'createdAt', 'streamingSession_createdAt' - The ISO timestamp in seconds for when the resource was created.
--
-- 'createdBy', 'streamingSession_createdBy' - The user ID of the user that created the streaming session.
--
-- 'ec2InstanceType', 'streamingSession_ec2InstanceType' - The EC2 Instance type used for the streaming session.
--
-- 'launchProfileId', 'streamingSession_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'maxBackupsToRetain', 'streamingSession_maxBackupsToRetain' - The maximum number of backups of a streaming session that you can have.
-- When the maximum number of backups is reached, the oldest backup is
-- deleted.
--
-- 'ownedBy', 'streamingSession_ownedBy' - The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
--
-- 'sessionId', 'streamingSession_sessionId' - The session ID.
--
-- 'sessionPersistenceMode', 'streamingSession_sessionPersistenceMode' - Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
--
-- 'startedAt', 'streamingSession_startedAt' - The time the session entered @START_IN_PROGRESS@ state.
--
-- 'startedBy', 'streamingSession_startedBy' - The user ID of the user that started the streaming session.
--
-- 'startedFromBackupId', 'streamingSession_startedFromBackupId' - The backup ID used to restore a streaming session.
--
-- 'state', 'streamingSession_state' - The current state.
--
-- 'statusCode', 'streamingSession_statusCode' - The status code.
--
-- 'statusMessage', 'streamingSession_statusMessage' - The status message for the streaming session.
--
-- 'stopAt', 'streamingSession_stopAt' - The time the streaming session will automatically be stopped if the user
-- doesn’t stop the session themselves.
--
-- 'stoppedAt', 'streamingSession_stoppedAt' - The time the session entered @STOP_IN_PROGRESS@ state.
--
-- 'stoppedBy', 'streamingSession_stoppedBy' - The user ID of the user that stopped the streaming session.
--
-- 'streamingImageId', 'streamingSession_streamingImageId' - The ID of the streaming image.
--
-- 'tags', 'streamingSession_tags' - A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
--
-- 'terminateAt', 'streamingSession_terminateAt' - The time the streaming session will automatically terminate if not
-- terminated by the user.
--
-- 'updatedAt', 'streamingSession_updatedAt' - The ISO timestamp in seconds for when the resource was updated.
--
-- 'updatedBy', 'streamingSession_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'volumeConfiguration', 'streamingSession_volumeConfiguration' - Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
--
-- 'volumeRetentionMode', 'streamingSession_volumeRetentionMode' - Determine if an EBS volume created from this streaming session will be
-- backed up.
newStreamingSession ::
  StreamingSession
newStreamingSession =
  StreamingSession'
    { arn = Prelude.Nothing,
      automaticTerminationMode = Prelude.Nothing,
      backupMode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      ec2InstanceType = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      maxBackupsToRetain = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      sessionPersistenceMode = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      startedFromBackupId = Prelude.Nothing,
      state = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      stopAt = Prelude.Nothing,
      stoppedAt = Prelude.Nothing,
      stoppedBy = Prelude.Nothing,
      streamingImageId = Prelude.Nothing,
      tags = Prelude.Nothing,
      terminateAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      volumeConfiguration = Prelude.Nothing,
      volumeRetentionMode = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
streamingSession_arn :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_arn = Lens.lens (\StreamingSession' {arn} -> arn) (\s@StreamingSession' {} a -> s {arn = a} :: StreamingSession)

-- | Indicates if a streaming session created from this launch profile should
-- be terminated automatically or retained without termination after being
-- in a @STOPPED@ state.
--
-- -   When @ACTIVATED@, the streaming session is scheduled for termination
--     after being in the @STOPPED@ state for the time specified in
--     @maxStoppedSessionLengthInMinutes@.
--
-- -   When @DEACTIVATED@, the streaming session can remain in the
--     @STOPPED@ state indefinitely.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@. When allowed, the default value for this parameter is
-- @DEACTIVATED@.
streamingSession_automaticTerminationMode :: Lens.Lens' StreamingSession (Prelude.Maybe AutomaticTerminationMode)
streamingSession_automaticTerminationMode = Lens.lens (\StreamingSession' {automaticTerminationMode} -> automaticTerminationMode) (\s@StreamingSession' {} a -> s {automaticTerminationMode = a} :: StreamingSession)

-- | Shows the current backup setting of the session.
streamingSession_backupMode :: Lens.Lens' StreamingSession (Prelude.Maybe SessionBackupMode)
streamingSession_backupMode = Lens.lens (\StreamingSession' {backupMode} -> backupMode) (\s@StreamingSession' {} a -> s {backupMode = a} :: StreamingSession)

-- | The ISO timestamp in seconds for when the resource was created.
streamingSession_createdAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_createdAt = Lens.lens (\StreamingSession' {createdAt} -> createdAt) (\s@StreamingSession' {} a -> s {createdAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The user ID of the user that created the streaming session.
streamingSession_createdBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_createdBy = Lens.lens (\StreamingSession' {createdBy} -> createdBy) (\s@StreamingSession' {} a -> s {createdBy = a} :: StreamingSession)

-- | The EC2 Instance type used for the streaming session.
streamingSession_ec2InstanceType :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_ec2InstanceType = Lens.lens (\StreamingSession' {ec2InstanceType} -> ec2InstanceType) (\s@StreamingSession' {} a -> s {ec2InstanceType = a} :: StreamingSession)

-- | The ID of the launch profile used to control access from the streaming
-- session.
streamingSession_launchProfileId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_launchProfileId = Lens.lens (\StreamingSession' {launchProfileId} -> launchProfileId) (\s@StreamingSession' {} a -> s {launchProfileId = a} :: StreamingSession)

-- | The maximum number of backups of a streaming session that you can have.
-- When the maximum number of backups is reached, the oldest backup is
-- deleted.
streamingSession_maxBackupsToRetain :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Natural)
streamingSession_maxBackupsToRetain = Lens.lens (\StreamingSession' {maxBackupsToRetain} -> maxBackupsToRetain) (\s@StreamingSession' {} a -> s {maxBackupsToRetain = a} :: StreamingSession)

-- | The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
streamingSession_ownedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_ownedBy = Lens.lens (\StreamingSession' {ownedBy} -> ownedBy) (\s@StreamingSession' {} a -> s {ownedBy = a} :: StreamingSession)

-- | The session ID.
streamingSession_sessionId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_sessionId = Lens.lens (\StreamingSession' {sessionId} -> sessionId) (\s@StreamingSession' {} a -> s {sessionId = a} :: StreamingSession)

-- | Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
streamingSession_sessionPersistenceMode :: Lens.Lens' StreamingSession (Prelude.Maybe SessionPersistenceMode)
streamingSession_sessionPersistenceMode = Lens.lens (\StreamingSession' {sessionPersistenceMode} -> sessionPersistenceMode) (\s@StreamingSession' {} a -> s {sessionPersistenceMode = a} :: StreamingSession)

-- | The time the session entered @START_IN_PROGRESS@ state.
streamingSession_startedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_startedAt = Lens.lens (\StreamingSession' {startedAt} -> startedAt) (\s@StreamingSession' {} a -> s {startedAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The user ID of the user that started the streaming session.
streamingSession_startedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_startedBy = Lens.lens (\StreamingSession' {startedBy} -> startedBy) (\s@StreamingSession' {} a -> s {startedBy = a} :: StreamingSession)

-- | The backup ID used to restore a streaming session.
streamingSession_startedFromBackupId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_startedFromBackupId = Lens.lens (\StreamingSession' {startedFromBackupId} -> startedFromBackupId) (\s@StreamingSession' {} a -> s {startedFromBackupId = a} :: StreamingSession)

-- | The current state.
streamingSession_state :: Lens.Lens' StreamingSession (Prelude.Maybe StreamingSessionState)
streamingSession_state = Lens.lens (\StreamingSession' {state} -> state) (\s@StreamingSession' {} a -> s {state = a} :: StreamingSession)

-- | The status code.
streamingSession_statusCode :: Lens.Lens' StreamingSession (Prelude.Maybe StreamingSessionStatusCode)
streamingSession_statusCode = Lens.lens (\StreamingSession' {statusCode} -> statusCode) (\s@StreamingSession' {} a -> s {statusCode = a} :: StreamingSession)

-- | The status message for the streaming session.
streamingSession_statusMessage :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_statusMessage = Lens.lens (\StreamingSession' {statusMessage} -> statusMessage) (\s@StreamingSession' {} a -> s {statusMessage = a} :: StreamingSession)

-- | The time the streaming session will automatically be stopped if the user
-- doesn’t stop the session themselves.
streamingSession_stopAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_stopAt = Lens.lens (\StreamingSession' {stopAt} -> stopAt) (\s@StreamingSession' {} a -> s {stopAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The time the session entered @STOP_IN_PROGRESS@ state.
streamingSession_stoppedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_stoppedAt = Lens.lens (\StreamingSession' {stoppedAt} -> stoppedAt) (\s@StreamingSession' {} a -> s {stoppedAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The user ID of the user that stopped the streaming session.
streamingSession_stoppedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_stoppedBy = Lens.lens (\StreamingSession' {stoppedBy} -> stoppedBy) (\s@StreamingSession' {} a -> s {stoppedBy = a} :: StreamingSession)

-- | The ID of the streaming image.
streamingSession_streamingImageId :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_streamingImageId = Lens.lens (\StreamingSession' {streamingImageId} -> streamingImageId) (\s@StreamingSession' {} a -> s {streamingImageId = a} :: StreamingSession)

-- | A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
streamingSession_tags :: Lens.Lens' StreamingSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamingSession_tags = Lens.lens (\StreamingSession' {tags} -> tags) (\s@StreamingSession' {} a -> s {tags = a} :: StreamingSession) Prelude.. Lens.mapping Lens.coerced

-- | The time the streaming session will automatically terminate if not
-- terminated by the user.
streamingSession_terminateAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_terminateAt = Lens.lens (\StreamingSession' {terminateAt} -> terminateAt) (\s@StreamingSession' {} a -> s {terminateAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The ISO timestamp in seconds for when the resource was updated.
streamingSession_updatedAt :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.UTCTime)
streamingSession_updatedAt = Lens.lens (\StreamingSession' {updatedAt} -> updatedAt) (\s@StreamingSession' {} a -> s {updatedAt = a} :: StreamingSession) Prelude.. Lens.mapping Data._Time

-- | The user ID of the user that most recently updated the resource.
streamingSession_updatedBy :: Lens.Lens' StreamingSession (Prelude.Maybe Prelude.Text)
streamingSession_updatedBy = Lens.lens (\StreamingSession' {updatedBy} -> updatedBy) (\s@StreamingSession' {} a -> s {updatedBy = a} :: StreamingSession)

-- | Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
streamingSession_volumeConfiguration :: Lens.Lens' StreamingSession (Prelude.Maybe VolumeConfiguration)
streamingSession_volumeConfiguration = Lens.lens (\StreamingSession' {volumeConfiguration} -> volumeConfiguration) (\s@StreamingSession' {} a -> s {volumeConfiguration = a} :: StreamingSession)

-- | Determine if an EBS volume created from this streaming session will be
-- backed up.
streamingSession_volumeRetentionMode :: Lens.Lens' StreamingSession (Prelude.Maybe VolumeRetentionMode)
streamingSession_volumeRetentionMode = Lens.lens (\StreamingSession' {volumeRetentionMode} -> volumeRetentionMode) (\s@StreamingSession' {} a -> s {volumeRetentionMode = a} :: StreamingSession)

instance Data.FromJSON StreamingSession where
  parseJSON =
    Data.withObject
      "StreamingSession"
      ( \x ->
          StreamingSession'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "automaticTerminationMode")
            Prelude.<*> (x Data..:? "backupMode")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "ec2InstanceType")
            Prelude.<*> (x Data..:? "launchProfileId")
            Prelude.<*> (x Data..:? "maxBackupsToRetain")
            Prelude.<*> (x Data..:? "ownedBy")
            Prelude.<*> (x Data..:? "sessionId")
            Prelude.<*> (x Data..:? "sessionPersistenceMode")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "startedBy")
            Prelude.<*> (x Data..:? "startedFromBackupId")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "stopAt")
            Prelude.<*> (x Data..:? "stoppedAt")
            Prelude.<*> (x Data..:? "stoppedBy")
            Prelude.<*> (x Data..:? "streamingImageId")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "terminateAt")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..:? "updatedBy")
            Prelude.<*> (x Data..:? "volumeConfiguration")
            Prelude.<*> (x Data..:? "volumeRetentionMode")
      )

instance Prelude.Hashable StreamingSession where
  hashWithSalt _salt StreamingSession' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` automaticTerminationMode
      `Prelude.hashWithSalt` backupMode
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` ec2InstanceType
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` maxBackupsToRetain
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` sessionPersistenceMode
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` startedFromBackupId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` stopAt
      `Prelude.hashWithSalt` stoppedAt
      `Prelude.hashWithSalt` stoppedBy
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` terminateAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` volumeConfiguration
      `Prelude.hashWithSalt` volumeRetentionMode

instance Prelude.NFData StreamingSession where
  rnf StreamingSession' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf automaticTerminationMode
      `Prelude.seq` Prelude.rnf backupMode
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf ec2InstanceType
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf maxBackupsToRetain
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf sessionPersistenceMode
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf startedFromBackupId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf stopAt
      `Prelude.seq` Prelude.rnf stoppedAt
      `Prelude.seq` Prelude.rnf stoppedBy
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf terminateAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf
        updatedBy
      `Prelude.seq` Prelude.rnf
        volumeConfiguration
      `Prelude.seq` Prelude.rnf
        volumeRetentionMode
