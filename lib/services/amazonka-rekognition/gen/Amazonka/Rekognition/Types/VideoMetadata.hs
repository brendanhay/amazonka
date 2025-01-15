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
-- Module      : Amazonka.Rekognition.Types.VideoMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.VideoMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.VideoColorRange

-- | Information about a video that Amazon Rekognition analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
--
-- /See:/ 'newVideoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { -- | Type of compression used in the analyzed video.
    codec :: Prelude.Maybe Prelude.Text,
    -- | A description of the range of luminance values in a video, either
    -- LIMITED (16 to 235) or FULL (0 to 255).
    colorRange :: Prelude.Maybe VideoColorRange,
    -- | Length of the video in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
    format :: Prelude.Maybe Prelude.Text,
    -- | Vertical pixel dimension of the video.
    frameHeight :: Prelude.Maybe Prelude.Natural,
    -- | Number of frames per second in the video.
    frameRate :: Prelude.Maybe Prelude.Double,
    -- | Horizontal pixel dimension of the video.
    frameWidth :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codec', 'videoMetadata_codec' - Type of compression used in the analyzed video.
--
-- 'colorRange', 'videoMetadata_colorRange' - A description of the range of luminance values in a video, either
-- LIMITED (16 to 235) or FULL (0 to 255).
--
-- 'durationMillis', 'videoMetadata_durationMillis' - Length of the video in milliseconds.
--
-- 'format', 'videoMetadata_format' - Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- 'frameHeight', 'videoMetadata_frameHeight' - Vertical pixel dimension of the video.
--
-- 'frameRate', 'videoMetadata_frameRate' - Number of frames per second in the video.
--
-- 'frameWidth', 'videoMetadata_frameWidth' - Horizontal pixel dimension of the video.
newVideoMetadata ::
  VideoMetadata
newVideoMetadata =
  VideoMetadata'
    { codec = Prelude.Nothing,
      colorRange = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      format = Prelude.Nothing,
      frameHeight = Prelude.Nothing,
      frameRate = Prelude.Nothing,
      frameWidth = Prelude.Nothing
    }

-- | Type of compression used in the analyzed video.
videoMetadata_codec :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Text)
videoMetadata_codec = Lens.lens (\VideoMetadata' {codec} -> codec) (\s@VideoMetadata' {} a -> s {codec = a} :: VideoMetadata)

-- | A description of the range of luminance values in a video, either
-- LIMITED (16 to 235) or FULL (0 to 255).
videoMetadata_colorRange :: Lens.Lens' VideoMetadata (Prelude.Maybe VideoColorRange)
videoMetadata_colorRange = Lens.lens (\VideoMetadata' {colorRange} -> colorRange) (\s@VideoMetadata' {} a -> s {colorRange = a} :: VideoMetadata)

-- | Length of the video in milliseconds.
videoMetadata_durationMillis :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Natural)
videoMetadata_durationMillis = Lens.lens (\VideoMetadata' {durationMillis} -> durationMillis) (\s@VideoMetadata' {} a -> s {durationMillis = a} :: VideoMetadata)

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
videoMetadata_format :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Text)
videoMetadata_format = Lens.lens (\VideoMetadata' {format} -> format) (\s@VideoMetadata' {} a -> s {format = a} :: VideoMetadata)

-- | Vertical pixel dimension of the video.
videoMetadata_frameHeight :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Natural)
videoMetadata_frameHeight = Lens.lens (\VideoMetadata' {frameHeight} -> frameHeight) (\s@VideoMetadata' {} a -> s {frameHeight = a} :: VideoMetadata)

-- | Number of frames per second in the video.
videoMetadata_frameRate :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Double)
videoMetadata_frameRate = Lens.lens (\VideoMetadata' {frameRate} -> frameRate) (\s@VideoMetadata' {} a -> s {frameRate = a} :: VideoMetadata)

-- | Horizontal pixel dimension of the video.
videoMetadata_frameWidth :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Natural)
videoMetadata_frameWidth = Lens.lens (\VideoMetadata' {frameWidth} -> frameWidth) (\s@VideoMetadata' {} a -> s {frameWidth = a} :: VideoMetadata)

instance Data.FromJSON VideoMetadata where
  parseJSON =
    Data.withObject
      "VideoMetadata"
      ( \x ->
          VideoMetadata'
            Prelude.<$> (x Data..:? "Codec")
            Prelude.<*> (x Data..:? "ColorRange")
            Prelude.<*> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "FrameHeight")
            Prelude.<*> (x Data..:? "FrameRate")
            Prelude.<*> (x Data..:? "FrameWidth")
      )

instance Prelude.Hashable VideoMetadata where
  hashWithSalt _salt VideoMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` colorRange
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` frameHeight
      `Prelude.hashWithSalt` frameRate
      `Prelude.hashWithSalt` frameWidth

instance Prelude.NFData VideoMetadata where
  rnf VideoMetadata' {..} =
    Prelude.rnf codec `Prelude.seq`
      Prelude.rnf colorRange `Prelude.seq`
        Prelude.rnf durationMillis `Prelude.seq`
          Prelude.rnf format `Prelude.seq`
            Prelude.rnf frameHeight `Prelude.seq`
              Prelude.rnf frameRate `Prelude.seq`
                Prelude.rnf frameWidth
