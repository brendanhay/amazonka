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
-- Module      : Amazonka.IVS.Types.RecordingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.RecordingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.DestinationConfiguration
import Amazonka.IVS.Types.RecordingConfigurationState
import Amazonka.IVS.Types.ThumbnailConfiguration
import qualified Amazonka.Prelude as Prelude

-- | An object representing a configuration to record a channel stream.
--
-- /See:/ 'newRecordingConfiguration' smart constructor.
data RecordingConfiguration = RecordingConfiguration'
  { -- | Recording-configuration name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | If a broadcast disconnects and then reconnects within the specified
    -- interval, the multiple streams will be considered a single broadcast and
    -- merged together. Default: 0.
    recordingReconnectWindowSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A complex type that allows you to enable\/disable the recording of
    -- thumbnails for a live session and modify the interval at which
    -- thumbnails are generated for the live session.
    thumbnailConfiguration :: Prelude.Maybe ThumbnailConfiguration,
    -- | Recording-configuration ARN.
    arn :: Prelude.Text,
    -- | A complex type that contains information about where recorded video will
    -- be stored.
    destinationConfiguration :: DestinationConfiguration,
    -- | Indicates the current state of the recording configuration. When the
    -- state is @ACTIVE@, the configuration is ready for recording a channel
    -- stream.
    state :: RecordingConfigurationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recordingConfiguration_name' - Recording-configuration name. The value does not need to be unique.
--
-- 'recordingReconnectWindowSeconds', 'recordingConfiguration_recordingReconnectWindowSeconds' - If a broadcast disconnects and then reconnects within the specified
-- interval, the multiple streams will be considered a single broadcast and
-- merged together. Default: 0.
--
-- 'tags', 'recordingConfiguration_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'thumbnailConfiguration', 'recordingConfiguration_thumbnailConfiguration' - A complex type that allows you to enable\/disable the recording of
-- thumbnails for a live session and modify the interval at which
-- thumbnails are generated for the live session.
--
-- 'arn', 'recordingConfiguration_arn' - Recording-configuration ARN.
--
-- 'destinationConfiguration', 'recordingConfiguration_destinationConfiguration' - A complex type that contains information about where recorded video will
-- be stored.
--
-- 'state', 'recordingConfiguration_state' - Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
newRecordingConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'destinationConfiguration'
  DestinationConfiguration ->
  -- | 'state'
  RecordingConfigurationState ->
  RecordingConfiguration
newRecordingConfiguration
  pArn_
  pDestinationConfiguration_
  pState_ =
    RecordingConfiguration'
      { name = Prelude.Nothing,
        recordingReconnectWindowSeconds = Prelude.Nothing,
        tags = Prelude.Nothing,
        thumbnailConfiguration = Prelude.Nothing,
        arn = pArn_,
        destinationConfiguration =
          pDestinationConfiguration_,
        state = pState_
      }

-- | Recording-configuration name. The value does not need to be unique.
recordingConfiguration_name :: Lens.Lens' RecordingConfiguration (Prelude.Maybe Prelude.Text)
recordingConfiguration_name = Lens.lens (\RecordingConfiguration' {name} -> name) (\s@RecordingConfiguration' {} a -> s {name = a} :: RecordingConfiguration)

-- | If a broadcast disconnects and then reconnects within the specified
-- interval, the multiple streams will be considered a single broadcast and
-- merged together. Default: 0.
recordingConfiguration_recordingReconnectWindowSeconds :: Lens.Lens' RecordingConfiguration (Prelude.Maybe Prelude.Natural)
recordingConfiguration_recordingReconnectWindowSeconds = Lens.lens (\RecordingConfiguration' {recordingReconnectWindowSeconds} -> recordingReconnectWindowSeconds) (\s@RecordingConfiguration' {} a -> s {recordingReconnectWindowSeconds = a} :: RecordingConfiguration)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
recordingConfiguration_tags :: Lens.Lens' RecordingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recordingConfiguration_tags = Lens.lens (\RecordingConfiguration' {tags} -> tags) (\s@RecordingConfiguration' {} a -> s {tags = a} :: RecordingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A complex type that allows you to enable\/disable the recording of
-- thumbnails for a live session and modify the interval at which
-- thumbnails are generated for the live session.
recordingConfiguration_thumbnailConfiguration :: Lens.Lens' RecordingConfiguration (Prelude.Maybe ThumbnailConfiguration)
recordingConfiguration_thumbnailConfiguration = Lens.lens (\RecordingConfiguration' {thumbnailConfiguration} -> thumbnailConfiguration) (\s@RecordingConfiguration' {} a -> s {thumbnailConfiguration = a} :: RecordingConfiguration)

-- | Recording-configuration ARN.
recordingConfiguration_arn :: Lens.Lens' RecordingConfiguration Prelude.Text
recordingConfiguration_arn = Lens.lens (\RecordingConfiguration' {arn} -> arn) (\s@RecordingConfiguration' {} a -> s {arn = a} :: RecordingConfiguration)

-- | A complex type that contains information about where recorded video will
-- be stored.
recordingConfiguration_destinationConfiguration :: Lens.Lens' RecordingConfiguration DestinationConfiguration
recordingConfiguration_destinationConfiguration = Lens.lens (\RecordingConfiguration' {destinationConfiguration} -> destinationConfiguration) (\s@RecordingConfiguration' {} a -> s {destinationConfiguration = a} :: RecordingConfiguration)

-- | Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
recordingConfiguration_state :: Lens.Lens' RecordingConfiguration RecordingConfigurationState
recordingConfiguration_state = Lens.lens (\RecordingConfiguration' {state} -> state) (\s@RecordingConfiguration' {} a -> s {state = a} :: RecordingConfiguration)

instance Data.FromJSON RecordingConfiguration where
  parseJSON =
    Data.withObject
      "RecordingConfiguration"
      ( \x ->
          RecordingConfiguration'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recordingReconnectWindowSeconds")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "thumbnailConfiguration")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "destinationConfiguration")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable RecordingConfiguration where
  hashWithSalt _salt RecordingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recordingReconnectWindowSeconds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` thumbnailConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` destinationConfiguration
      `Prelude.hashWithSalt` state

instance Prelude.NFData RecordingConfiguration where
  rnf RecordingConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf recordingReconnectWindowSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf thumbnailConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf state
