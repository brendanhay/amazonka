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
import Network.AWS.MediaConvert.Types.Hdr10Plus
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.TimecodeBurnin
import qualified Network.AWS.Prelude as Prelude

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- /See:/ 'newVideoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { -- | Enable the Image inserter (ImageInserter) feature to include a graphic
    -- overlay on your video. Enable or disable this feature for each output
    -- individually. This setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Settings for burning the output timecode and specified prefix into the
    -- output.
    timecodeBurnin :: Prelude.Maybe TimecodeBurnin,
    -- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
    -- your video output if necessary. Enable or disable this feature for each
    -- output individually. This setting is disabled by default.
    noiseReducer :: Prelude.Maybe NoiseReducer,
    -- | Use the deinterlacer to produce smoother motion and a clearer picture.
    -- For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
    deinterlacer :: Prelude.Maybe Deinterlacer,
    -- | Use these settings to convert the color space or to modify properties
    -- such as hue and contrast for this output. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
    colorCorrector :: Prelude.Maybe ColorCorrector,
    -- | If you work with a third party video watermarking partner, use the group
    -- of settings that correspond with your watermarking partner to include
    -- watermarks in your output.
    partnerWatermarking :: Prelude.Maybe PartnerWatermarking,
    -- | Enable Dolby Vision feature to produce Dolby Vision compatible video
    -- output.
    dolbyVision :: Prelude.Maybe DolbyVision,
    -- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
    hdr10Plus :: Prelude.Maybe Hdr10Plus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'timecodeBurnin', 'videoPreprocessor_timecodeBurnin' - Settings for burning the output timecode and specified prefix into the
-- output.
--
-- 'noiseReducer', 'videoPreprocessor_noiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
--
-- 'deinterlacer', 'videoPreprocessor_deinterlacer' - Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
--
-- 'colorCorrector', 'videoPreprocessor_colorCorrector' - Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
--
-- 'partnerWatermarking', 'videoPreprocessor_partnerWatermarking' - If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
--
-- 'dolbyVision', 'videoPreprocessor_dolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
--
-- 'hdr10Plus', 'videoPreprocessor_hdr10Plus' - Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
newVideoPreprocessor ::
  VideoPreprocessor
newVideoPreprocessor =
  VideoPreprocessor'
    { imageInserter = Prelude.Nothing,
      timecodeBurnin = Prelude.Nothing,
      noiseReducer = Prelude.Nothing,
      deinterlacer = Prelude.Nothing,
      colorCorrector = Prelude.Nothing,
      partnerWatermarking = Prelude.Nothing,
      dolbyVision = Prelude.Nothing,
      hdr10Plus = Prelude.Nothing
    }

-- | Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
videoPreprocessor_imageInserter :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ImageInserter)
videoPreprocessor_imageInserter = Lens.lens (\VideoPreprocessor' {imageInserter} -> imageInserter) (\s@VideoPreprocessor' {} a -> s {imageInserter = a} :: VideoPreprocessor)

-- | Settings for burning the output timecode and specified prefix into the
-- output.
videoPreprocessor_timecodeBurnin :: Lens.Lens' VideoPreprocessor (Prelude.Maybe TimecodeBurnin)
videoPreprocessor_timecodeBurnin = Lens.lens (\VideoPreprocessor' {timecodeBurnin} -> timecodeBurnin) (\s@VideoPreprocessor' {} a -> s {timecodeBurnin = a} :: VideoPreprocessor)

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
videoPreprocessor_noiseReducer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe NoiseReducer)
videoPreprocessor_noiseReducer = Lens.lens (\VideoPreprocessor' {noiseReducer} -> noiseReducer) (\s@VideoPreprocessor' {} a -> s {noiseReducer = a} :: VideoPreprocessor)

-- | Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
videoPreprocessor_deinterlacer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Deinterlacer)
videoPreprocessor_deinterlacer = Lens.lens (\VideoPreprocessor' {deinterlacer} -> deinterlacer) (\s@VideoPreprocessor' {} a -> s {deinterlacer = a} :: VideoPreprocessor)

-- | Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
videoPreprocessor_colorCorrector :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ColorCorrector)
videoPreprocessor_colorCorrector = Lens.lens (\VideoPreprocessor' {colorCorrector} -> colorCorrector) (\s@VideoPreprocessor' {} a -> s {colorCorrector = a} :: VideoPreprocessor)

-- | If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
videoPreprocessor_partnerWatermarking :: Lens.Lens' VideoPreprocessor (Prelude.Maybe PartnerWatermarking)
videoPreprocessor_partnerWatermarking = Lens.lens (\VideoPreprocessor' {partnerWatermarking} -> partnerWatermarking) (\s@VideoPreprocessor' {} a -> s {partnerWatermarking = a} :: VideoPreprocessor)

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
videoPreprocessor_dolbyVision :: Lens.Lens' VideoPreprocessor (Prelude.Maybe DolbyVision)
videoPreprocessor_dolbyVision = Lens.lens (\VideoPreprocessor' {dolbyVision} -> dolbyVision) (\s@VideoPreprocessor' {} a -> s {dolbyVision = a} :: VideoPreprocessor)

-- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
videoPreprocessor_hdr10Plus :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Hdr10Plus)
videoPreprocessor_hdr10Plus = Lens.lens (\VideoPreprocessor' {hdr10Plus} -> hdr10Plus) (\s@VideoPreprocessor' {} a -> s {hdr10Plus = a} :: VideoPreprocessor)

instance Core.FromJSON VideoPreprocessor where
  parseJSON =
    Core.withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            Prelude.<$> (x Core..:? "imageInserter")
            Prelude.<*> (x Core..:? "timecodeBurnin")
            Prelude.<*> (x Core..:? "noiseReducer")
            Prelude.<*> (x Core..:? "deinterlacer")
            Prelude.<*> (x Core..:? "colorCorrector")
            Prelude.<*> (x Core..:? "partnerWatermarking")
            Prelude.<*> (x Core..:? "dolbyVision")
            Prelude.<*> (x Core..:? "hdr10Plus")
      )

instance Prelude.Hashable VideoPreprocessor

instance Prelude.NFData VideoPreprocessor

instance Core.ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("imageInserter" Core..=) Prelude.<$> imageInserter,
            ("timecodeBurnin" Core..=)
              Prelude.<$> timecodeBurnin,
            ("noiseReducer" Core..=) Prelude.<$> noiseReducer,
            ("deinterlacer" Core..=) Prelude.<$> deinterlacer,
            ("colorCorrector" Core..=)
              Prelude.<$> colorCorrector,
            ("partnerWatermarking" Core..=)
              Prelude.<$> partnerWatermarking,
            ("dolbyVision" Core..=) Prelude.<$> dolbyVision,
            ("hdr10Plus" Core..=) Prelude.<$> hdr10Plus
          ]
      )
