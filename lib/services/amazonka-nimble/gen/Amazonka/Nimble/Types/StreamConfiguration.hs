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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StreamConfigurationSessionStorage
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingInstanceType
import qualified Amazonka.Prelude as Prelude

-- | A configuration for a streaming session.
--
-- /See:/ 'newStreamConfiguration' smart constructor.
data StreamConfiguration = StreamConfiguration'
  { -- | Integer that determines if you can start and stop your sessions and how
    -- long a session can stay in the STOPPED state. The default value is 0.
    -- The maximum value is 5760.
    --
    -- If the value is missing or set to 0, your sessions can’t be stopped. If
    -- you then call @StopStreamingSession@, the session fails. If the time
    -- that a session stays in the READY state exceeds the
    -- @maxSessionLengthInMinutes@ value, the session will automatically be
    -- terminated (instead of stopped).
    --
    -- If the value is set to a positive number, the session can be stopped.
    -- You can call @StopStreamingSession@ to stop sessions in the READY state.
    -- If the time that a session stays in the READY state exceeds the
    -- @maxSessionLengthInMinutes@ value, the session will automatically be
    -- stopped (instead of terminated).
    maxStoppedSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The length of time, in minutes, that a streaming session can be active
    -- before it is stopped or terminated. After this point, Nimble Studio
    -- automatically terminates or stops the session. The default length of
    -- time is 690 minutes, and the maximum length of time is 30 days.
    maxSessionLengthInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) The upload storage for a streaming session.
    sessionStorage :: Prelude.Maybe StreamConfigurationSessionStorage,
    -- | Enable or disable the use of the system clipboard to copy and paste
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
-- 'maxStoppedSessionLengthInMinutes', 'streamConfiguration_maxStoppedSessionLengthInMinutes' - Integer that determines if you can start and stop your sessions and how
-- long a session can stay in the STOPPED state. The default value is 0.
-- The maximum value is 5760.
--
-- If the value is missing or set to 0, your sessions can’t be stopped. If
-- you then call @StopStreamingSession@, the session fails. If the time
-- that a session stays in the READY state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- terminated (instead of stopped).
--
-- If the value is set to a positive number, the session can be stopped.
-- You can call @StopStreamingSession@ to stop sessions in the READY state.
-- If the time that a session stays in the READY state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- stopped (instead of terminated).
--
-- 'maxSessionLengthInMinutes', 'streamConfiguration_maxSessionLengthInMinutes' - The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
--
-- 'sessionStorage', 'streamConfiguration_sessionStorage' - (Optional) The upload storage for a streaming session.
--
-- 'clipboardMode', 'streamConfiguration_clipboardMode' - Enable or disable the use of the system clipboard to copy and paste
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
      { maxStoppedSessionLengthInMinutes =
          Prelude.Nothing,
        maxSessionLengthInMinutes = Prelude.Nothing,
        sessionStorage = Prelude.Nothing,
        clipboardMode = pClipboardMode_,
        ec2InstanceTypes =
          Lens.coerced Lens.# pEc2InstanceTypes_,
        streamingImageIds =
          Lens.coerced Lens.# pStreamingImageIds_
      }

-- | Integer that determines if you can start and stop your sessions and how
-- long a session can stay in the STOPPED state. The default value is 0.
-- The maximum value is 5760.
--
-- If the value is missing or set to 0, your sessions can’t be stopped. If
-- you then call @StopStreamingSession@, the session fails. If the time
-- that a session stays in the READY state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- terminated (instead of stopped).
--
-- If the value is set to a positive number, the session can be stopped.
-- You can call @StopStreamingSession@ to stop sessions in the READY state.
-- If the time that a session stays in the READY state exceeds the
-- @maxSessionLengthInMinutes@ value, the session will automatically be
-- stopped (instead of terminated).
streamConfiguration_maxStoppedSessionLengthInMinutes :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Natural)
streamConfiguration_maxStoppedSessionLengthInMinutes = Lens.lens (\StreamConfiguration' {maxStoppedSessionLengthInMinutes} -> maxStoppedSessionLengthInMinutes) (\s@StreamConfiguration' {} a -> s {maxStoppedSessionLengthInMinutes = a} :: StreamConfiguration)

-- | The length of time, in minutes, that a streaming session can be active
-- before it is stopped or terminated. After this point, Nimble Studio
-- automatically terminates or stops the session. The default length of
-- time is 690 minutes, and the maximum length of time is 30 days.
streamConfiguration_maxSessionLengthInMinutes :: Lens.Lens' StreamConfiguration (Prelude.Maybe Prelude.Natural)
streamConfiguration_maxSessionLengthInMinutes = Lens.lens (\StreamConfiguration' {maxSessionLengthInMinutes} -> maxSessionLengthInMinutes) (\s@StreamConfiguration' {} a -> s {maxSessionLengthInMinutes = a} :: StreamConfiguration)

-- | (Optional) The upload storage for a streaming session.
streamConfiguration_sessionStorage :: Lens.Lens' StreamConfiguration (Prelude.Maybe StreamConfigurationSessionStorage)
streamConfiguration_sessionStorage = Lens.lens (\StreamConfiguration' {sessionStorage} -> sessionStorage) (\s@StreamConfiguration' {} a -> s {sessionStorage = a} :: StreamConfiguration)

-- | Enable or disable the use of the system clipboard to copy and paste
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
            Prelude.<$> (x Data..:? "maxStoppedSessionLengthInMinutes")
            Prelude.<*> (x Data..:? "maxSessionLengthInMinutes")
            Prelude.<*> (x Data..:? "sessionStorage")
            Prelude.<*> (x Data..: "clipboardMode")
            Prelude.<*> (x Data..: "ec2InstanceTypes")
            Prelude.<*> (x Data..: "streamingImageIds")
      )

instance Prelude.Hashable StreamConfiguration where
  hashWithSalt _salt StreamConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` maxStoppedSessionLengthInMinutes
      `Prelude.hashWithSalt` maxSessionLengthInMinutes
      `Prelude.hashWithSalt` sessionStorage
      `Prelude.hashWithSalt` clipboardMode
      `Prelude.hashWithSalt` ec2InstanceTypes
      `Prelude.hashWithSalt` streamingImageIds

instance Prelude.NFData StreamConfiguration where
  rnf StreamConfiguration' {..} =
    Prelude.rnf maxStoppedSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf maxSessionLengthInMinutes
      `Prelude.seq` Prelude.rnf sessionStorage
      `Prelude.seq` Prelude.rnf clipboardMode
      `Prelude.seq` Prelude.rnf ec2InstanceTypes
      `Prelude.seq` Prelude.rnf streamingImageIds
