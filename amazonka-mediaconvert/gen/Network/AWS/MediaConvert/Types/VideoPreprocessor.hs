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
-- Module      : Network.AWS.MediaConvert.Types.VideoPreprocessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoPreprocessor where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.Deinterlacer
import Network.AWS.MediaConvert.Types.DolbyVision
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.TimecodeBurnin

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- /See:/ 'newVideoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { -- | Enable the Image inserter (ImageInserter) feature to include a graphic
    -- overlay on your video. Enable or disable this feature for each output
    -- individually. This setting is disabled by default.
    imageInserter :: Core.Maybe ImageInserter,
    -- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and
    -- specified prefix into the output.
    timecodeBurnin :: Core.Maybe TimecodeBurnin,
    -- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer
    -- picture.
    deinterlacer :: Core.Maybe Deinterlacer,
    -- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
    -- your video output if necessary. Enable or disable this feature for each
    -- output individually. This setting is disabled by default.
    noiseReducer :: Core.Maybe NoiseReducer,
    -- | If you work with a third party video watermarking partner, use the group
    -- of settings that correspond with your watermarking partner to include
    -- watermarks in your output.
    partnerWatermarking :: Core.Maybe PartnerWatermarking,
    -- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable
    -- or disable this feature for each output individually. This setting is
    -- disabled by default.
    colorCorrector :: Core.Maybe ColorCorrector,
    -- | Enable Dolby Vision feature to produce Dolby Vision compatible video
    -- output.
    dolbyVision :: Core.Maybe DolbyVision
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VideoPreprocessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageInserter', 'videoPreprocessor_imageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
--
-- 'timecodeBurnin', 'videoPreprocessor_timecodeBurnin' - Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and
-- specified prefix into the output.
--
-- 'deinterlacer', 'videoPreprocessor_deinterlacer' - Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer
-- picture.
--
-- 'noiseReducer', 'videoPreprocessor_noiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
--
-- 'partnerWatermarking', 'videoPreprocessor_partnerWatermarking' - If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
--
-- 'colorCorrector', 'videoPreprocessor_colorCorrector' - Enable the Color corrector (ColorCorrector) feature if necessary. Enable
-- or disable this feature for each output individually. This setting is
-- disabled by default.
--
-- 'dolbyVision', 'videoPreprocessor_dolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
newVideoPreprocessor ::
  VideoPreprocessor
newVideoPreprocessor =
  VideoPreprocessor'
    { imageInserter = Core.Nothing,
      timecodeBurnin = Core.Nothing,
      deinterlacer = Core.Nothing,
      noiseReducer = Core.Nothing,
      partnerWatermarking = Core.Nothing,
      colorCorrector = Core.Nothing,
      dolbyVision = Core.Nothing
    }

-- | Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
videoPreprocessor_imageInserter :: Lens.Lens' VideoPreprocessor (Core.Maybe ImageInserter)
videoPreprocessor_imageInserter = Lens.lens (\VideoPreprocessor' {imageInserter} -> imageInserter) (\s@VideoPreprocessor' {} a -> s {imageInserter = a} :: VideoPreprocessor)

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and
-- specified prefix into the output.
videoPreprocessor_timecodeBurnin :: Lens.Lens' VideoPreprocessor (Core.Maybe TimecodeBurnin)
videoPreprocessor_timecodeBurnin = Lens.lens (\VideoPreprocessor' {timecodeBurnin} -> timecodeBurnin) (\s@VideoPreprocessor' {} a -> s {timecodeBurnin = a} :: VideoPreprocessor)

-- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer
-- picture.
videoPreprocessor_deinterlacer :: Lens.Lens' VideoPreprocessor (Core.Maybe Deinterlacer)
videoPreprocessor_deinterlacer = Lens.lens (\VideoPreprocessor' {deinterlacer} -> deinterlacer) (\s@VideoPreprocessor' {} a -> s {deinterlacer = a} :: VideoPreprocessor)

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
videoPreprocessor_noiseReducer :: Lens.Lens' VideoPreprocessor (Core.Maybe NoiseReducer)
videoPreprocessor_noiseReducer = Lens.lens (\VideoPreprocessor' {noiseReducer} -> noiseReducer) (\s@VideoPreprocessor' {} a -> s {noiseReducer = a} :: VideoPreprocessor)

-- | If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
videoPreprocessor_partnerWatermarking :: Lens.Lens' VideoPreprocessor (Core.Maybe PartnerWatermarking)
videoPreprocessor_partnerWatermarking = Lens.lens (\VideoPreprocessor' {partnerWatermarking} -> partnerWatermarking) (\s@VideoPreprocessor' {} a -> s {partnerWatermarking = a} :: VideoPreprocessor)

-- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable
-- or disable this feature for each output individually. This setting is
-- disabled by default.
videoPreprocessor_colorCorrector :: Lens.Lens' VideoPreprocessor (Core.Maybe ColorCorrector)
videoPreprocessor_colorCorrector = Lens.lens (\VideoPreprocessor' {colorCorrector} -> colorCorrector) (\s@VideoPreprocessor' {} a -> s {colorCorrector = a} :: VideoPreprocessor)

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
videoPreprocessor_dolbyVision :: Lens.Lens' VideoPreprocessor (Core.Maybe DolbyVision)
videoPreprocessor_dolbyVision = Lens.lens (\VideoPreprocessor' {dolbyVision} -> dolbyVision) (\s@VideoPreprocessor' {} a -> s {dolbyVision = a} :: VideoPreprocessor)

instance Core.FromJSON VideoPreprocessor where
  parseJSON =
    Core.withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            Core.<$> (x Core..:? "imageInserter")
            Core.<*> (x Core..:? "timecodeBurnin")
            Core.<*> (x Core..:? "deinterlacer")
            Core.<*> (x Core..:? "noiseReducer")
            Core.<*> (x Core..:? "partnerWatermarking")
            Core.<*> (x Core..:? "colorCorrector")
            Core.<*> (x Core..:? "dolbyVision")
      )

instance Core.Hashable VideoPreprocessor

instance Core.NFData VideoPreprocessor

instance Core.ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("imageInserter" Core..=) Core.<$> imageInserter,
            ("timecodeBurnin" Core..=) Core.<$> timecodeBurnin,
            ("deinterlacer" Core..=) Core.<$> deinterlacer,
            ("noiseReducer" Core..=) Core.<$> noiseReducer,
            ("partnerWatermarking" Core..=)
              Core.<$> partnerWatermarking,
            ("colorCorrector" Core..=) Core.<$> colorCorrector,
            ("dolbyVision" Core..=) Core.<$> dolbyVision
          ]
      )
