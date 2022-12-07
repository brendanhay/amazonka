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
-- Module      : Amazonka.IVS.Types.ThumbnailConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.ThumbnailConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.RecordingMode
import qualified Amazonka.Prelude as Prelude

-- | An object representing a configuration of thumbnails for recorded video.
--
-- /See:/ 'newThumbnailConfiguration' smart constructor.
data ThumbnailConfiguration = ThumbnailConfiguration'
  { -- | Thumbnail recording mode. Default: @INTERVAL@.
    recordingMode :: Prelude.Maybe RecordingMode,
    -- | The targeted thumbnail-generation interval in seconds. This is
    -- configurable (and required) only if @recordingMode@ is @INTERVAL@.
    -- Default: 60.
    --
    -- __Important:__ Setting a value for @targetIntervalSeconds@ does not
    -- guarantee that thumbnails are generated at the specified interval. For
    -- thumbnails to be generated at the @targetIntervalSeconds@ interval, the
    -- @IDR\/Keyframe@ value for the input video must be less than the
    -- @targetIntervalSeconds@ value. See
    -- <https://docs.aws.amazon.com/ivs/latest/userguide/streaming-config.html Amazon IVS Streaming Configuration>
    -- for information on setting @IDR\/Keyframe@ to the recommended value in
    -- video-encoder settings.
    targetIntervalSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThumbnailConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordingMode', 'thumbnailConfiguration_recordingMode' - Thumbnail recording mode. Default: @INTERVAL@.
--
-- 'targetIntervalSeconds', 'thumbnailConfiguration_targetIntervalSeconds' - The targeted thumbnail-generation interval in seconds. This is
-- configurable (and required) only if @recordingMode@ is @INTERVAL@.
-- Default: 60.
--
-- __Important:__ Setting a value for @targetIntervalSeconds@ does not
-- guarantee that thumbnails are generated at the specified interval. For
-- thumbnails to be generated at the @targetIntervalSeconds@ interval, the
-- @IDR\/Keyframe@ value for the input video must be less than the
-- @targetIntervalSeconds@ value. See
-- <https://docs.aws.amazon.com/ivs/latest/userguide/streaming-config.html Amazon IVS Streaming Configuration>
-- for information on setting @IDR\/Keyframe@ to the recommended value in
-- video-encoder settings.
newThumbnailConfiguration ::
  ThumbnailConfiguration
newThumbnailConfiguration =
  ThumbnailConfiguration'
    { recordingMode =
        Prelude.Nothing,
      targetIntervalSeconds = Prelude.Nothing
    }

-- | Thumbnail recording mode. Default: @INTERVAL@.
thumbnailConfiguration_recordingMode :: Lens.Lens' ThumbnailConfiguration (Prelude.Maybe RecordingMode)
thumbnailConfiguration_recordingMode = Lens.lens (\ThumbnailConfiguration' {recordingMode} -> recordingMode) (\s@ThumbnailConfiguration' {} a -> s {recordingMode = a} :: ThumbnailConfiguration)

-- | The targeted thumbnail-generation interval in seconds. This is
-- configurable (and required) only if @recordingMode@ is @INTERVAL@.
-- Default: 60.
--
-- __Important:__ Setting a value for @targetIntervalSeconds@ does not
-- guarantee that thumbnails are generated at the specified interval. For
-- thumbnails to be generated at the @targetIntervalSeconds@ interval, the
-- @IDR\/Keyframe@ value for the input video must be less than the
-- @targetIntervalSeconds@ value. See
-- <https://docs.aws.amazon.com/ivs/latest/userguide/streaming-config.html Amazon IVS Streaming Configuration>
-- for information on setting @IDR\/Keyframe@ to the recommended value in
-- video-encoder settings.
thumbnailConfiguration_targetIntervalSeconds :: Lens.Lens' ThumbnailConfiguration (Prelude.Maybe Prelude.Natural)
thumbnailConfiguration_targetIntervalSeconds = Lens.lens (\ThumbnailConfiguration' {targetIntervalSeconds} -> targetIntervalSeconds) (\s@ThumbnailConfiguration' {} a -> s {targetIntervalSeconds = a} :: ThumbnailConfiguration)

instance Data.FromJSON ThumbnailConfiguration where
  parseJSON =
    Data.withObject
      "ThumbnailConfiguration"
      ( \x ->
          ThumbnailConfiguration'
            Prelude.<$> (x Data..:? "recordingMode")
            Prelude.<*> (x Data..:? "targetIntervalSeconds")
      )

instance Prelude.Hashable ThumbnailConfiguration where
  hashWithSalt _salt ThumbnailConfiguration' {..} =
    _salt `Prelude.hashWithSalt` recordingMode
      `Prelude.hashWithSalt` targetIntervalSeconds

instance Prelude.NFData ThumbnailConfiguration where
  rnf ThumbnailConfiguration' {..} =
    Prelude.rnf recordingMode
      `Prelude.seq` Prelude.rnf targetIntervalSeconds

instance Data.ToJSON ThumbnailConfiguration where
  toJSON ThumbnailConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recordingMode" Data..=) Prelude.<$> recordingMode,
            ("targetIntervalSeconds" Data..=)
              Prelude.<$> targetIntervalSeconds
          ]
      )
