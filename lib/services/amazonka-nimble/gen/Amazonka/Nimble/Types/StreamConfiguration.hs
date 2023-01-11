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
-- Module      : Amazonka.Nimble.Types.StreamConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfiguration where

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

-- | A configuration for a streaming session.
--
-- /See:/ 'newStreamConfiguration' smart constructor.
data StreamConfiguration = StreamConfiguration'
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
    -- | Information about the streaming session backup.
    sessionBackup :: Prelude.Maybe StreamConfigurationSessionBackup,
    -- | Determine if a streaming session created from this launch profile can
    -- configure persistent storage. This means that @volumeConfiguration@ and
    -- @automaticTerminationMode@ are configured.
    sessionPersistenceMode :: Prelude.Maybe SessionPersistenceMode,
    -- | The upload storage for a streaming session.
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
-- Create a value of 'StreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticTerminationMode', 'streamConfiguration_automaticTerminationMode' - Indicates if a streaming session created from this launch profile should
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
-- 'maxSessionLengthInMinutes', 'streamConfiguration_maxSessionLengthInMinutes' - The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
--
-- 'maxStoppedSessionLengthInMinutes', 'streamConfiguration_maxStoppedSessionLengthInMinutes' - Integer that determines if you can start and stop your sessions and how
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
-- 'sessionBackup', 'streamConfiguration_sessionBackup' - Information about the streaming session backup.
--
-- 'sessionPersistenceMode', 'streamConfiguration_sessionPersistenceMode' - Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
--
-- 'sessionStorage', 'streamConfiguration_sessionStorage' - The upload storage for a streaming session.
--
-- 'volumeConfiguration', 'streamConfiguration_volumeConfiguration' - Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
--
-- 'clipboardMode', 'streamConfiguration_clipboardMode' - Allows or deactivates the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
--
-- 'ec2InstanceTypes', 'streamConfiguration_ec2InstanceTypes' - The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
--
-- 'streamingImageIds', 'streamConfiguration_streamingImageIds' - The streaming images that users can select from when launching a
-- streaming session with this launch profile.
newStreamConfiguration ::
  -- | 'clipboardMode'
  StreamingClipboardMode ->
  -- | 'ec2InstanceTypes'
  Prelude.NonEmpty StreamingInstanceType ->
  -- | 'streamingImageIds'
  Prelude.NonEmpty Prelude.Text ->
  StreamConfiguration
newStreamConfiguration
  pClipboardMode_
  pEc2InstanceTypes_
  pStreamingImageIds_ =
    StreamConfiguration'
      { automaticTerminationMode =
          Prelude.Nothing,
        maxSessionLengthInMinutes = Prelude.Nothing,
        maxStoppedSessionLengthInMinutes = Prelude.Nothing,
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
streamConfiguration_automaticTerminationMode :: Lens.Lens' StreamConfiguration (Prelude.Maybe AutomaticTerminationMode)
streamConfiguration_automaticTerminationMode = Lens.lens (\StreamConfiguration' {automaticTerminationMode} -> automaticTerminationMode) (\s@StreamConfiguration' {} a -> s {automaticTerminationMode = a} :: StreamConfiguration)

-- | The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
streamConfiguration_maxSessionLengthInMinutes :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Natural)
streamConfiguration_maxSessionLengthInMinutes = Lens.lens (\StreamConfiguration' {maxSessionLengthInMinutes} -> maxSessionLengthInMinutes) (\s@StreamConfiguration' {} a -> s {maxSessionLengthInMinutes = a} :: StreamConfiguration)

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
streamConfiguration_maxStoppedSessionLengthInMinutes :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Natural)
streamConfiguration_maxStoppedSessionLengthInMinutes = Lens.lens (\StreamConfiguration' {maxStoppedSessionLengthInMinutes} -> maxStoppedSessionLengthInMinutes) (\s@StreamConfiguration' {} a -> s {maxStoppedSessionLengthInMinutes = a} :: StreamConfiguration)

-- | Information about the streaming session backup.
streamConfiguration_sessionBackup :: Lens.Lens' StreamConfiguration (Prelude.Maybe StreamConfigurationSessionBackup)
streamConfiguration_sessionBackup = Lens.lens (\StreamConfiguration' {sessionBackup} -> sessionBackup) (\s@StreamConfiguration' {} a -> s {sessionBackup = a} :: StreamConfiguration)

-- | Determine if a streaming session created from this launch profile can
-- configure persistent storage. This means that @volumeConfiguration@ and
-- @automaticTerminationMode@ are configured.
streamConfiguration_sessionPersistenceMode :: Lens.Lens' StreamConfiguration (Prelude.Maybe SessionPersistenceMode)
streamConfiguration_sessionPersistenceMode = Lens.lens (\StreamConfiguration' {sessionPersistenceMode} -> sessionPersistenceMode) (\s@StreamConfiguration' {} a -> s {sessionPersistenceMode = a} :: StreamConfiguration)

-- | The upload storage for a streaming session.
streamConfiguration_sessionStorage :: Lens.Lens' StreamConfiguration (Prelude.Maybe StreamConfigurationSessionStorage)
streamConfiguration_sessionStorage = Lens.lens (\StreamConfiguration' {sessionStorage} -> sessionStorage) (\s@StreamConfiguration' {} a -> s {sessionStorage = a} :: StreamConfiguration)

-- | Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
streamConfiguration_volumeConfiguration :: Lens.Lens' StreamConfiguration (Prelude.Maybe VolumeConfiguration)
streamConfiguration_volumeConfiguration = Lens.lens (\StreamConfiguration' {volumeConfiguration} -> volumeConfiguration) (\s@StreamConfiguration' {} a -> s {volumeConfiguration = a} :: StreamConfiguration)

-- | Allows or deactivates the use of the system clipboard to copy and paste
-- between the streaming session and streaming client.
streamConfiguration_clipboardMode :: Lens.Lens' StreamConfiguration StreamingClipboardMode
streamConfiguration_clipboardMode = Lens.lens (\StreamConfiguration' {clipboardMode} -> clipboardMode) (\s@StreamConfiguration' {} a -> s {clipboardMode = a} :: StreamConfiguration)

-- | The EC2 instance types that users can select from when launching a
-- streaming session with this launch profile.
streamConfiguration_ec2InstanceTypes :: Lens.Lens' StreamConfiguration (Prelude.NonEmpty StreamingInstanceType)
streamConfiguration_ec2InstanceTypes = Lens.lens (\StreamConfiguration' {ec2InstanceTypes} -> ec2InstanceTypes) (\s@StreamConfiguration' {} a -> s {ec2InstanceTypes = a} :: StreamConfiguration) Prelude.. Lens.coerced

-- | The streaming images that users can select from when launching a
-- streaming session with this launch profile.
streamConfiguration_streamingImageIds :: Lens.Lens' StreamConfiguration (Prelude.NonEmpty Prelude.Text)
streamConfiguration_streamingImageIds = Lens.lens (\StreamConfiguration' {streamingImageIds} -> streamingImageIds) (\s@StreamConfiguration' {} a -> s {streamingImageIds = a} :: StreamConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON StreamConfiguration where
  parseJSON =
    Data.withObject
      "StreamConfiguration"
      ( \x ->
          StreamConfiguration'
            Prelude.<$> (x Data..:? "automaticTerminationMode")
            Prelude.<*> (x Data..:? "maxSessionLengthInMinutes")
            Prelude.<*> (x Data..:? "maxStoppedSessionLengthInMinutes")
            Prelude.<*> (x Data..:? "sessionBackup")
            Prelude.<*> (x Data..:? "sessionPersistenceMode")
            Prelude.<*> (x Data..:? "sessionStorage")
            Prelude.<*> (x Data..:? "volumeConfiguration")
            Prelude.<*> (x Data..: "clipboardMode")
            Prelude.<*> (x Data..: "ec2InstanceTypes")
            Prelude.<*> (x Data..: "streamingImageIds")
      )

instance Prelude.Hashable StreamConfiguration where
  hashWithSalt _salt StreamConfiguration' {..} =
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

instance Prelude.NFData StreamConfiguration where
  rnf StreamConfiguration' {..} =
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
