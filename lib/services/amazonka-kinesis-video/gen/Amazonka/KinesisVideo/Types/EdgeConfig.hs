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
-- Module      : Amazonka.KinesisVideo.Types.EdgeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.EdgeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.DeletionConfig
import Amazonka.KinesisVideo.Types.RecorderConfig
import Amazonka.KinesisVideo.Types.UploaderConfig
import qualified Amazonka.Prelude as Prelude

-- | A description of the stream\'s edge configuration that will be used to
-- sync with the Edge Agent IoT Greengrass component. The Edge Agent
-- component will run on an IoT Hub Device setup at your premise.
--
-- /See:/ 'newEdgeConfig' smart constructor.
data EdgeConfig = EdgeConfig'
  { -- | The deletion configuration is made up of the retention time
    -- (@EdgeRetentionInHours@) and local size configuration
    -- (@LocalSizeConfig@) details that are used to make the deletion.
    deletionConfig :: Prelude.Maybe DeletionConfig,
    -- | The uploader configuration contains the @ScheduleExpression@ details
    -- that are used to schedule upload jobs for the recorded media files from
    -- the Edge Agent to a Kinesis Video Stream.
    uploaderConfig :: Prelude.Maybe UploaderConfig,
    -- | The \"__Internet of Things (IoT) Thing__\" Arn of the stream.
    hubDeviceArn :: Prelude.Text,
    -- | The recorder configuration consists of the local @MediaSourceConfig@
    -- details, that are used as credentials to access the local media files
    -- streamed on the camera.
    recorderConfig :: RecorderConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionConfig', 'edgeConfig_deletionConfig' - The deletion configuration is made up of the retention time
-- (@EdgeRetentionInHours@) and local size configuration
-- (@LocalSizeConfig@) details that are used to make the deletion.
--
-- 'uploaderConfig', 'edgeConfig_uploaderConfig' - The uploader configuration contains the @ScheduleExpression@ details
-- that are used to schedule upload jobs for the recorded media files from
-- the Edge Agent to a Kinesis Video Stream.
--
-- 'hubDeviceArn', 'edgeConfig_hubDeviceArn' - The \"__Internet of Things (IoT) Thing__\" Arn of the stream.
--
-- 'recorderConfig', 'edgeConfig_recorderConfig' - The recorder configuration consists of the local @MediaSourceConfig@
-- details, that are used as credentials to access the local media files
-- streamed on the camera.
newEdgeConfig ::
  -- | 'hubDeviceArn'
  Prelude.Text ->
  -- | 'recorderConfig'
  RecorderConfig ->
  EdgeConfig
newEdgeConfig pHubDeviceArn_ pRecorderConfig_ =
  EdgeConfig'
    { deletionConfig = Prelude.Nothing,
      uploaderConfig = Prelude.Nothing,
      hubDeviceArn = pHubDeviceArn_,
      recorderConfig = pRecorderConfig_
    }

-- | The deletion configuration is made up of the retention time
-- (@EdgeRetentionInHours@) and local size configuration
-- (@LocalSizeConfig@) details that are used to make the deletion.
edgeConfig_deletionConfig :: Lens.Lens' EdgeConfig (Prelude.Maybe DeletionConfig)
edgeConfig_deletionConfig = Lens.lens (\EdgeConfig' {deletionConfig} -> deletionConfig) (\s@EdgeConfig' {} a -> s {deletionConfig = a} :: EdgeConfig)

-- | The uploader configuration contains the @ScheduleExpression@ details
-- that are used to schedule upload jobs for the recorded media files from
-- the Edge Agent to a Kinesis Video Stream.
edgeConfig_uploaderConfig :: Lens.Lens' EdgeConfig (Prelude.Maybe UploaderConfig)
edgeConfig_uploaderConfig = Lens.lens (\EdgeConfig' {uploaderConfig} -> uploaderConfig) (\s@EdgeConfig' {} a -> s {uploaderConfig = a} :: EdgeConfig)

-- | The \"__Internet of Things (IoT) Thing__\" Arn of the stream.
edgeConfig_hubDeviceArn :: Lens.Lens' EdgeConfig Prelude.Text
edgeConfig_hubDeviceArn = Lens.lens (\EdgeConfig' {hubDeviceArn} -> hubDeviceArn) (\s@EdgeConfig' {} a -> s {hubDeviceArn = a} :: EdgeConfig)

-- | The recorder configuration consists of the local @MediaSourceConfig@
-- details, that are used as credentials to access the local media files
-- streamed on the camera.
edgeConfig_recorderConfig :: Lens.Lens' EdgeConfig RecorderConfig
edgeConfig_recorderConfig = Lens.lens (\EdgeConfig' {recorderConfig} -> recorderConfig) (\s@EdgeConfig' {} a -> s {recorderConfig = a} :: EdgeConfig)

instance Data.FromJSON EdgeConfig where
  parseJSON =
    Data.withObject
      "EdgeConfig"
      ( \x ->
          EdgeConfig'
            Prelude.<$> (x Data..:? "DeletionConfig")
            Prelude.<*> (x Data..:? "UploaderConfig")
            Prelude.<*> (x Data..: "HubDeviceArn")
            Prelude.<*> (x Data..: "RecorderConfig")
      )

instance Prelude.Hashable EdgeConfig where
  hashWithSalt _salt EdgeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` deletionConfig
      `Prelude.hashWithSalt` uploaderConfig
      `Prelude.hashWithSalt` hubDeviceArn
      `Prelude.hashWithSalt` recorderConfig

instance Prelude.NFData EdgeConfig where
  rnf EdgeConfig' {..} =
    Prelude.rnf deletionConfig `Prelude.seq`
      Prelude.rnf uploaderConfig `Prelude.seq`
        Prelude.rnf hubDeviceArn `Prelude.seq`
          Prelude.rnf recorderConfig

instance Data.ToJSON EdgeConfig where
  toJSON EdgeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeletionConfig" Data..=)
              Prelude.<$> deletionConfig,
            ("UploaderConfig" Data..=)
              Prelude.<$> uploaderConfig,
            Prelude.Just ("HubDeviceArn" Data..= hubDeviceArn),
            Prelude.Just
              ("RecorderConfig" Data..= recorderConfig)
          ]
      )
