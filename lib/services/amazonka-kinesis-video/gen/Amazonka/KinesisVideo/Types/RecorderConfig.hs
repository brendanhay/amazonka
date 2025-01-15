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
-- Module      : Amazonka.KinesisVideo.Types.RecorderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.RecorderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.MediaSourceConfig
import Amazonka.KinesisVideo.Types.ScheduleConfig
import qualified Amazonka.Prelude as Prelude

-- | The recorder configuration consists of the local @MediaSourceConfig@
-- details that are used as credentials to accesss the local media files
-- streamed on the camera.
--
-- /See:/ 'newRecorderConfig' smart constructor.
data RecorderConfig = RecorderConfig'
  { -- | The configuration that consists of the @ScheduleExpression@ and the
    -- @DurationInMinutes@ details that specify the scheduling to record from a
    -- camera, or local media file, onto the Edge Agent. If the
    -- @ScheduleExpression@ attribute is not provided, then the Edge Agent will
    -- always be set to recording mode.
    scheduleConfig :: Prelude.Maybe ScheduleConfig,
    -- | The configuration details that consist of the credentials required
    -- (@MediaUriSecretArn@ and @MediaUriType@) to access the media files
    -- streamed to the camera.
    mediaSourceConfig :: MediaSourceConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecorderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleConfig', 'recorderConfig_scheduleConfig' - The configuration that consists of the @ScheduleExpression@ and the
-- @DurationInMinutes@ details that specify the scheduling to record from a
-- camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ attribute is not provided, then the Edge Agent will
-- always be set to recording mode.
--
-- 'mediaSourceConfig', 'recorderConfig_mediaSourceConfig' - The configuration details that consist of the credentials required
-- (@MediaUriSecretArn@ and @MediaUriType@) to access the media files
-- streamed to the camera.
newRecorderConfig ::
  -- | 'mediaSourceConfig'
  MediaSourceConfig ->
  RecorderConfig
newRecorderConfig pMediaSourceConfig_ =
  RecorderConfig'
    { scheduleConfig = Prelude.Nothing,
      mediaSourceConfig = pMediaSourceConfig_
    }

-- | The configuration that consists of the @ScheduleExpression@ and the
-- @DurationInMinutes@ details that specify the scheduling to record from a
-- camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ attribute is not provided, then the Edge Agent will
-- always be set to recording mode.
recorderConfig_scheduleConfig :: Lens.Lens' RecorderConfig (Prelude.Maybe ScheduleConfig)
recorderConfig_scheduleConfig = Lens.lens (\RecorderConfig' {scheduleConfig} -> scheduleConfig) (\s@RecorderConfig' {} a -> s {scheduleConfig = a} :: RecorderConfig)

-- | The configuration details that consist of the credentials required
-- (@MediaUriSecretArn@ and @MediaUriType@) to access the media files
-- streamed to the camera.
recorderConfig_mediaSourceConfig :: Lens.Lens' RecorderConfig MediaSourceConfig
recorderConfig_mediaSourceConfig = Lens.lens (\RecorderConfig' {mediaSourceConfig} -> mediaSourceConfig) (\s@RecorderConfig' {} a -> s {mediaSourceConfig = a} :: RecorderConfig)

instance Data.FromJSON RecorderConfig where
  parseJSON =
    Data.withObject
      "RecorderConfig"
      ( \x ->
          RecorderConfig'
            Prelude.<$> (x Data..:? "ScheduleConfig")
            Prelude.<*> (x Data..: "MediaSourceConfig")
      )

instance Prelude.Hashable RecorderConfig where
  hashWithSalt _salt RecorderConfig' {..} =
    _salt
      `Prelude.hashWithSalt` scheduleConfig
      `Prelude.hashWithSalt` mediaSourceConfig

instance Prelude.NFData RecorderConfig where
  rnf RecorderConfig' {..} =
    Prelude.rnf scheduleConfig `Prelude.seq`
      Prelude.rnf mediaSourceConfig

instance Data.ToJSON RecorderConfig where
  toJSON RecorderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScheduleConfig" Data..=)
              Prelude.<$> scheduleConfig,
            Prelude.Just
              ("MediaSourceConfig" Data..= mediaSourceConfig)
          ]
      )
