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
-- Module      : Amazonka.Nimble.Types.StreamConfigurationCreate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfigurationCreate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.AutomaticTerminationMode
import Amazonka.Nimble.Types.SessionPersistenceMode
import Amazonka.Nimble.Types.StreamConfigurationSessionBackup
import Amazonka.Nimble.Types.StreamConfigurationSessionStorage
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingInstanceType
import Amazonka.Nimble.Types.VolumeConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration for streaming workstations created using this launch
-- profile.
--
-- /See:/ 'newStreamConfigurationCreate' smart constructor.
data StreamConfigurationCreate = StreamConfigurationCreate'
  { -- | Indicates if a streaming session created from this launch profile should
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
    -- | The length of time, in minutes, that a streaming session can be active
    -- before it is stopped or terminated. After this point, Nimble Studio
    -- automatically terminates or stops the session. The default length of
    -- time is 690 minutes, and the maximum length of time is 30 days.
    maxSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Integer that determines if you can start and stop your sessions and how
    -- long a session can stay in the @STOPPED@ state. The default value is 0.
    -- The maximum value is 5760.
    --
    -- This field is allowed only when @sessionPersistenceMode@ is @ACTIVATED@
    -- and @automaticTerminationMode@ is @ACTIVATED@.
    --
    -- If the value is set to 0, your sessions can’t be @STOPPED@. If you then
    -- call @StopStreamingSession@, the session fails. If the time that a
    -- session stays in the @READY@ state exceeds the
    -- @maxSessionLengthInMinutes@ value, the session will automatically be
    -- terminated (instead of @STOPPED@).
    --
    -- If the value is set to a positive number, the session can be stopped.
    -- You can call @StopStreamingSession@ to stop sessions in the @READY@
    -- state. If the time that a session stays in the @READY@ state exceeds the
    -- @maxSessionLengthInMinutes@ value, the session will automatically be
    -- stopped (instead of terminated).
    maxStoppedSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Configures how streaming sessions are backed up when launched from this
    -- launch profile.
    sessionBackup :: Prelude.Maybe StreamConfigurationSessionBackup,
    -- | Determine if a streaming session created from this launch profile can
    -- configure persistent storage. This means that @volumeConfiguration@ and
    -- @automaticTerminationMode@ are configured.
    sessionPersistenceMode :: Prelude.Maybe SessionPersistenceMode,
    -- | The upload storage for a streaming workstation that is created using
    -- this launch profile.
    sessionStorage :: Prelude.Maybe StreamConfigurationSessionStorage,
    -- | Custom volume configuration for the root volumes that are attached to
    -- streaming sessions.
    --
    -- This parameter is only allowed when @sessionPersistenceMode@ is
    -- @ACTIVATED@.
    volumeConfiguration :: Prelude.Maybe VolumeConfiguration,
    -- | Allows or deactivates the use of the system clipboard to copy and paste
    -- between the streaming session and streaming client.
    clipboardMode :: StreamingClipboardMode,
    -- | The EC2 instance types that users can select from when launching a
    -- streaming session with this launch profile.
    ec2InstanceTypes :: Prelude.NonEmpty StreamingInstanceType,
    -- | The streaming images that users can select from when launching a
    -- streaming session with this launch profile.
    streamingImageIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamConfigurationCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticTerminationMode', 'streamConfigurationCreate_automaticTerminationMode' - Indicates if a streaming session created from this launch profile should
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
-- 'maxSessionLengthInMinutes', 'streamConfigurationCreate_maxSessionLengthInMinutes' - The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
--
-- 'maxStoppedSessionLengthInMinutes', 'streamConfigurationCreate_maxStoppedSessionLengthInMinutes' - Integer that determines if you can start and stop your sessions and how
-- long a session can stay in the @STOPPED@ state. The default value is 0.
-- The maximum value is 5760.
--
-- This field is allowed only when @sessionPersistenceMode@ is @ACTIVATED@
-- and @automaticTerminationMode@ is @ACTIVATED@.
--
-- If the value is set to 0, your sessions can’t be @STOPPED@. If you then
-- call @StopStreamingSession@, the session fails. If the time that a
-- session stays in the @READY@ state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- terminated (instead of @STOPPED@).
--
-- If the value is set to a positive number, the session can be stopped.
-- You can call @StopStreamingSession@ to stop sessions in the @READY@
-- state. If the time that a session stays in the @READY@ state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- stopped (instead of terminated).
--
-- 'sessionBackup', 'streamConfigurationCreate_sessionBackup' - Configures how streaming sessions are backed up when launched from this
-- launch profile.
--
-- 'sessionPersistenceMode', 'streamConfigurationCreate_sessionPersistenceMode' - Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
--
-- 'sessionStorage', 'streamConfigurationCreate_sessionStorage' - The upload storage for a streaming workstation that is created using
-- this launch profile.
--
-- 'volumeConfiguration', 'streamConfigurationCreate_volumeConfiguration' - Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
--
-- 'clipboardMode', 'streamConfigurationCreate_clipboardMode' - Allows or deactivates the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
--
-- 'ec2InstanceTypes', 'streamConfigurationCreate_ec2InstanceTypes' - The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
--
-- 'streamingImageIds', 'streamConfigurationCreate_streamingImageIds' - The streaming images that users can select from when launching a
-- streaming session with this launch profile.
newStreamConfigurationCreate ::
  -- | 'clipboardMode'
  StreamingClipboardMode ->
  -- | 'ec2InstanceTypes'
  Prelude.NonEmpty StreamingInstanceType ->
  -- | 'streamingImageIds'
  Prelude.NonEmpty Prelude.Text ->
  StreamConfigurationCreate
newStreamConfigurationCreate
  pClipboardMode_
  pEc2InstanceTypes_
  pStreamingImageIds_ =
    StreamConfigurationCreate'
      { automaticTerminationMode =
          Prelude.Nothing,
        maxSessionLengthInMinutes = Prelude.Nothing,
        maxStoppedSessionLengthInMinutes =
          Prelude.Nothing,
        sessionBackup = Prelude.Nothing,
        sessionPersistenceMode = Prelude.Nothing,
        sessionStorage = Prelude.Nothing,
        volumeConfiguration = Prelude.Nothing,
        clipboardMode = pClipboardMode_,
        ec2InstanceTypes =
          Lens.coerced Lens.# pEc2InstanceTypes_,
        streamingImageIds =
          Lens.coerced Lens.# pStreamingImageIds_
      }

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
streamConfigurationCreate_automaticTerminationMode :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe AutomaticTerminationMode)
streamConfigurationCreate_automaticTerminationMode = Lens.lens (\StreamConfigurationCreate' {automaticTerminationMode} -> automaticTerminationMode) (\s@StreamConfigurationCreate' {} a -> s {automaticTerminationMode = a} :: StreamConfigurationCreate)

-- | The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
streamConfigurationCreate_maxSessionLengthInMinutes :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe Prelude.Natural)
streamConfigurationCreate_maxSessionLengthInMinutes = Lens.lens (\StreamConfigurationCreate' {maxSessionLengthInMinutes} -> maxSessionLengthInMinutes) (\s@StreamConfigurationCreate' {} a -> s {maxSessionLengthInMinutes = a} :: StreamConfigurationCreate)

-- | Integer that determines if you can start and stop your sessions and how
-- long a session can stay in the @STOPPED@ state. The default value is 0.
-- The maximum value is 5760.
--
-- This field is allowed only when @sessionPersistenceMode@ is @ACTIVATED@
-- and @automaticTerminationMode@ is @ACTIVATED@.
--
-- If the value is set to 0, your sessions can’t be @STOPPED@. If you then
-- call @StopStreamingSession@, the session fails. If the time that a
-- session stays in the @READY@ state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- terminated (instead of @STOPPED@).
--
-- If the value is set to a positive number, the session can be stopped.
-- You can call @StopStreamingSession@ to stop sessions in the @READY@
-- state. If the time that a session stays in the @READY@ state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- stopped (instead of terminated).
streamConfigurationCreate_maxStoppedSessionLengthInMinutes :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe Prelude.Natural)
streamConfigurationCreate_maxStoppedSessionLengthInMinutes = Lens.lens (\StreamConfigurationCreate' {maxStoppedSessionLengthInMinutes} -> maxStoppedSessionLengthInMinutes) (\s@StreamConfigurationCreate' {} a -> s {maxStoppedSessionLengthInMinutes = a} :: StreamConfigurationCreate)

-- | Configures how streaming sessions are backed up when launched from this
-- launch profile.
streamConfigurationCreate_sessionBackup :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe StreamConfigurationSessionBackup)
streamConfigurationCreate_sessionBackup = Lens.lens (\StreamConfigurationCreate' {sessionBackup} -> sessionBackup) (\s@StreamConfigurationCreate' {} a -> s {sessionBackup = a} :: StreamConfigurationCreate)

-- | Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
streamConfigurationCreate_sessionPersistenceMode :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe SessionPersistenceMode)
streamConfigurationCreate_sessionPersistenceMode = Lens.lens (\StreamConfigurationCreate' {sessionPersistenceMode} -> sessionPersistenceMode) (\s@StreamConfigurationCreate' {} a -> s {sessionPersistenceMode = a} :: StreamConfigurationCreate)

-- | The upload storage for a streaming workstation that is created using
-- this launch profile.
streamConfigurationCreate_sessionStorage :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe StreamConfigurationSessionStorage)
streamConfigurationCreate_sessionStorage = Lens.lens (\StreamConfigurationCreate' {sessionStorage} -> sessionStorage) (\s@StreamConfigurationCreate' {} a -> s {sessionStorage = a} :: StreamConfigurationCreate)

-- | Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
streamConfigurationCreate_volumeConfiguration :: Lens.Lens' StreamConfigurationCreate (Prelude.Maybe VolumeConfiguration)
streamConfigurationCreate_volumeConfiguration = Lens.lens (\StreamConfigurationCreate' {volumeConfiguration} -> volumeConfiguration) (\s@StreamConfigurationCreate' {} a -> s {volumeConfiguration = a} :: StreamConfigurationCreate)

-- | Allows or deactivates the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
streamConfigurationCreate_clipboardMode :: Lens.Lens' StreamConfigurationCreate StreamingClipboardMode
streamConfigurationCreate_clipboardMode = Lens.lens (\StreamConfigurationCreate' {clipboardMode} -> clipboardMode) (\s@StreamConfigurationCreate' {} a -> s {clipboardMode = a} :: StreamConfigurationCreate)

-- | The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
streamConfigurationCreate_ec2InstanceTypes :: Lens.Lens' StreamConfigurationCreate (Prelude.NonEmpty StreamingInstanceType)
streamConfigurationCreate_ec2InstanceTypes = Lens.lens (\StreamConfigurationCreate' {ec2InstanceTypes} -> ec2InstanceTypes) (\s@StreamConfigurationCreate' {} a -> s {ec2InstanceTypes = a} :: StreamConfigurationCreate) Prelude.. Lens.coerced

-- | The streaming images that users can select from when launching a
-- streaming session with this launch profile.
streamConfigurationCreate_streamingImageIds :: Lens.Lens' StreamConfigurationCreate (Prelude.NonEmpty Prelude.Text)
streamConfigurationCreate_streamingImageIds = Lens.lens (\StreamConfigurationCreate' {streamingImageIds} -> streamingImageIds) (\s@StreamConfigurationCreate' {} a -> s {streamingImageIds = a} :: StreamConfigurationCreate) Prelude.. Lens.coerced

instance Prelude.Hashable StreamConfigurationCreate where
  hashWithSalt _salt StreamConfigurationCreate' {..} =
    _salt
      `Prelude.hashWithSalt` automaticTerminationMode
      `Prelude.hashWithSalt` maxSessionLengthInMinutes
      `Prelude.hashWithSalt` maxStoppedSessionLengthInMinutes
      `Prelude.hashWithSalt` sessionBackup
      `Prelude.hashWithSalt` sessionPersistenceMode
      `Prelude.hashWithSalt` sessionStorage
      `Prelude.hashWithSalt` volumeConfiguration
      `Prelude.hashWithSalt` clipboardMode
      `Prelude.hashWithSalt` ec2InstanceTypes
      `Prelude.hashWithSalt` streamingImageIds

instance Prelude.NFData StreamConfigurationCreate where
  rnf StreamConfigurationCreate' {..} =
    Prelude.rnf automaticTerminationMode
      `Prelude.seq` Prelude.rnf maxSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf maxStoppedSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf sessionBackup
      `Prelude.seq` Prelude.rnf sessionPersistenceMode
      `Prelude.seq` Prelude.rnf sessionStorage
      `Prelude.seq` Prelude.rnf volumeConfiguration
      `Prelude.seq` Prelude.rnf clipboardMode
      `Prelude.seq` Prelude.rnf ec2InstanceTypes
      `Prelude.seq` Prelude.rnf streamingImageIds

instance Data.ToJSON StreamConfigurationCreate where
  toJSON StreamConfigurationCreate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("automaticTerminationMode" Data..=)
              Prelude.<$> automaticTerminationMode,
            ("maxSessionLengthInMinutes" Data..=)
              Prelude.<$> maxSessionLengthInMinutes,
            ("maxStoppedSessionLengthInMinutes" Data..=)
              Prelude.<$> maxStoppedSessionLengthInMinutes,
            ("sessionBackup" Data..=) Prelude.<$> sessionBackup,
            ("sessionPersistenceMode" Data..=)
              Prelude.<$> sessionPersistenceMode,
            ("sessionStorage" Data..=)
              Prelude.<$> sessionStorage,
            ("volumeConfiguration" Data..=)
              Prelude.<$> volumeConfiguration,
            Prelude.Just ("clipboardMode" Data..= clipboardMode),
            Prelude.Just
              ("ec2InstanceTypes" Data..= ec2InstanceTypes),
            Prelude.Just
              ("streamingImageIds" Data..= streamingImageIds)
          ]
      )
