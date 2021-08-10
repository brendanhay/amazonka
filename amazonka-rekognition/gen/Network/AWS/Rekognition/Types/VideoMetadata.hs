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
-- Module      : Network.AWS.Rekognition.Types.VideoMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.VideoMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a video that Amazon Rekognition analyzed.
-- @Videometadata@ is returned in every page of paginated responses from a
-- Amazon Rekognition video operation.
--
-- /See:/ 'newVideoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { -- | Type of compression used in the analyzed video.
    codec :: Prelude.Maybe Prelude.Text,
    -- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
    format :: Prelude.Maybe Prelude.Text,
    -- | Vertical pixel dimension of the video.
    frameHeight :: Prelude.Maybe Prelude.Natural,
    -- | Number of frames per second in the video.
    frameRate :: Prelude.Maybe Prelude.Double,
    -- | Horizontal pixel dimension of the video.
    frameWidth :: Prelude.Maybe Prelude.Natural,
    -- | Length of the video in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural
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
-- 'format', 'videoMetadata_format' - Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- 'frameHeight', 'videoMetadata_frameHeight' - Vertical pixel dimension of the video.
--
-- 'frameRate', 'videoMetadata_frameRate' - Number of frames per second in the video.
--
-- 'frameWidth', 'videoMetadata_frameWidth' - Horizontal pixel dimension of the video.
--
-- 'durationMillis', 'videoMetadata_durationMillis' - Length of the video in milliseconds.
newVideoMetadata ::
  VideoMetadata
newVideoMetadata =
  VideoMetadata'
    { codec = Prelude.Nothing,
      format = Prelude.Nothing,
      frameHeight = Prelude.Nothing,
      frameRate = Prelude.Nothing,
      frameWidth = Prelude.Nothing,
      durationMillis = Prelude.Nothing
    }

-- | Type of compression used in the analyzed video.
videoMetadata_codec :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Text)
videoMetadata_codec = Lens.lens (\VideoMetadata' {codec} -> codec) (\s@VideoMetadata' {} a -> s {codec = a} :: VideoMetadata)

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

-- | Length of the video in milliseconds.
videoMetadata_durationMillis :: Lens.Lens' VideoMetadata (Prelude.Maybe Prelude.Natural)
videoMetadata_durationMillis = Lens.lens (\VideoMetadata' {durationMillis} -> durationMillis) (\s@VideoMetadata' {} a -> s {durationMillis = a} :: VideoMetadata)

instance Core.FromJSON VideoMetadata where
  parseJSON =
    Core.withObject
      "VideoMetadata"
      ( \x ->
          VideoMetadata'
            Prelude.<$> (x Core..:? "Codec")
            Prelude.<*> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "FrameHeight")
            Prelude.<*> (x Core..:? "FrameRate")
            Prelude.<*> (x Core..:? "FrameWidth")
            Prelude.<*> (x Core..:? "DurationMillis")
      )

instance Prelude.Hashable VideoMetadata

instance Prelude.NFData VideoMetadata
