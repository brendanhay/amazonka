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
-- Module      : Amazonka.MediaConvert.Types.VideoPreprocessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoPreprocessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.ColorCorrector
import Amazonka.MediaConvert.Types.Deinterlacer
import Amazonka.MediaConvert.Types.DolbyVision
import Amazonka.MediaConvert.Types.Hdr10Plus
import Amazonka.MediaConvert.Types.ImageInserter
import Amazonka.MediaConvert.Types.NoiseReducer
import Amazonka.MediaConvert.Types.PartnerWatermarking
import Amazonka.MediaConvert.Types.TimecodeBurnin
import qualified Amazonka.Prelude as Prelude

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- /See:/ 'newVideoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { -- | Enable Dolby Vision feature to produce Dolby Vision compatible video
    -- output.
    dolbyVision :: Prelude.Maybe DolbyVision,
    -- | If you work with a third party video watermarking partner, use the group
    -- of settings that correspond with your watermarking partner to include
    -- watermarks in your output.
    partnerWatermarking :: Prelude.Maybe PartnerWatermarking,
    -- | Use these settings to convert the color space or to modify properties
    -- such as hue and contrast for this output. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
    colorCorrector :: Prelude.Maybe ColorCorrector,
    -- | Settings for burning the output timecode and specified prefix into the
    -- output.
    timecodeBurnin :: Prelude.Maybe TimecodeBurnin,
    -- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
    hdr10Plus :: Prelude.Maybe Hdr10Plus,
    -- | Enable the Image inserter (ImageInserter) feature to include a graphic
    -- overlay on your video. Enable or disable this feature for each output
    -- individually. This setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Use the deinterlacer to produce smoother motion and a clearer picture.
    -- For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
    deinterlacer :: Prelude.Maybe Deinterlacer,
    -- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
    -- your video output if necessary. Enable or disable this feature for each
    -- output individually. This setting is disabled by default.
    noiseReducer :: Prelude.Maybe NoiseReducer
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
-- 'dolbyVision', 'videoPreprocessor_dolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
--
-- 'partnerWatermarking', 'videoPreprocessor_partnerWatermarking' - If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
--
-- 'colorCorrector', 'videoPreprocessor_colorCorrector' - Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
--
-- 'timecodeBurnin', 'videoPreprocessor_timecodeBurnin' - Settings for burning the output timecode and specified prefix into the
-- output.
--
-- 'hdr10Plus', 'videoPreprocessor_hdr10Plus' - Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
--
-- 'imageInserter', 'videoPreprocessor_imageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
--
-- 'deinterlacer', 'videoPreprocessor_deinterlacer' - Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
--
-- 'noiseReducer', 'videoPreprocessor_noiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
newVideoPreprocessor ::
  VideoPreprocessor
newVideoPreprocessor =
  VideoPreprocessor'
    { dolbyVision = Prelude.Nothing,
      partnerWatermarking = Prelude.Nothing,
      colorCorrector = Prelude.Nothing,
      timecodeBurnin = Prelude.Nothing,
      hdr10Plus = Prelude.Nothing,
      imageInserter = Prelude.Nothing,
      deinterlacer = Prelude.Nothing,
      noiseReducer = Prelude.Nothing
    }

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
videoPreprocessor_dolbyVision :: Lens.Lens' VideoPreprocessor (Prelude.Maybe DolbyVision)
videoPreprocessor_dolbyVision = Lens.lens (\VideoPreprocessor' {dolbyVision} -> dolbyVision) (\s@VideoPreprocessor' {} a -> s {dolbyVision = a} :: VideoPreprocessor)

-- | If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
videoPreprocessor_partnerWatermarking :: Lens.Lens' VideoPreprocessor (Prelude.Maybe PartnerWatermarking)
videoPreprocessor_partnerWatermarking = Lens.lens (\VideoPreprocessor' {partnerWatermarking} -> partnerWatermarking) (\s@VideoPreprocessor' {} a -> s {partnerWatermarking = a} :: VideoPreprocessor)

-- | Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
videoPreprocessor_colorCorrector :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ColorCorrector)
videoPreprocessor_colorCorrector = Lens.lens (\VideoPreprocessor' {colorCorrector} -> colorCorrector) (\s@VideoPreprocessor' {} a -> s {colorCorrector = a} :: VideoPreprocessor)

-- | Settings for burning the output timecode and specified prefix into the
-- output.
videoPreprocessor_timecodeBurnin :: Lens.Lens' VideoPreprocessor (Prelude.Maybe TimecodeBurnin)
videoPreprocessor_timecodeBurnin = Lens.lens (\VideoPreprocessor' {timecodeBurnin} -> timecodeBurnin) (\s@VideoPreprocessor' {} a -> s {timecodeBurnin = a} :: VideoPreprocessor)

-- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
videoPreprocessor_hdr10Plus :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Hdr10Plus)
videoPreprocessor_hdr10Plus = Lens.lens (\VideoPreprocessor' {hdr10Plus} -> hdr10Plus) (\s@VideoPreprocessor' {} a -> s {hdr10Plus = a} :: VideoPreprocessor)

-- | Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
videoPreprocessor_imageInserter :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ImageInserter)
videoPreprocessor_imageInserter = Lens.lens (\VideoPreprocessor' {imageInserter} -> imageInserter) (\s@VideoPreprocessor' {} a -> s {imageInserter = a} :: VideoPreprocessor)

-- | Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
videoPreprocessor_deinterlacer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Deinterlacer)
videoPreprocessor_deinterlacer = Lens.lens (\VideoPreprocessor' {deinterlacer} -> deinterlacer) (\s@VideoPreprocessor' {} a -> s {deinterlacer = a} :: VideoPreprocessor)

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default.
videoPreprocessor_noiseReducer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe NoiseReducer)
videoPreprocessor_noiseReducer = Lens.lens (\VideoPreprocessor' {noiseReducer} -> noiseReducer) (\s@VideoPreprocessor' {} a -> s {noiseReducer = a} :: VideoPreprocessor)

instance Core.FromJSON VideoPreprocessor where
  parseJSON =
    Core.withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            Prelude.<$> (x Core..:? "dolbyVision")
            Prelude.<*> (x Core..:? "partnerWatermarking")
            Prelude.<*> (x Core..:? "colorCorrector")
            Prelude.<*> (x Core..:? "timecodeBurnin")
            Prelude.<*> (x Core..:? "hdr10Plus")
            Prelude.<*> (x Core..:? "imageInserter")
            Prelude.<*> (x Core..:? "deinterlacer")
            Prelude.<*> (x Core..:? "noiseReducer")
      )

instance Prelude.Hashable VideoPreprocessor where
  hashWithSalt _salt VideoPreprocessor' {..} =
    _salt `Prelude.hashWithSalt` dolbyVision
      `Prelude.hashWithSalt` partnerWatermarking
      `Prelude.hashWithSalt` colorCorrector
      `Prelude.hashWithSalt` timecodeBurnin
      `Prelude.hashWithSalt` hdr10Plus
      `Prelude.hashWithSalt` imageInserter
      `Prelude.hashWithSalt` deinterlacer
      `Prelude.hashWithSalt` noiseReducer

instance Prelude.NFData VideoPreprocessor where
  rnf VideoPreprocessor' {..} =
    Prelude.rnf dolbyVision
      `Prelude.seq` Prelude.rnf partnerWatermarking
      `Prelude.seq` Prelude.rnf colorCorrector
      `Prelude.seq` Prelude.rnf timecodeBurnin
      `Prelude.seq` Prelude.rnf hdr10Plus
      `Prelude.seq` Prelude.rnf imageInserter
      `Prelude.seq` Prelude.rnf deinterlacer
      `Prelude.seq` Prelude.rnf noiseReducer

instance Core.ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dolbyVision" Core..=) Prelude.<$> dolbyVision,
            ("partnerWatermarking" Core..=)
              Prelude.<$> partnerWatermarking,
            ("colorCorrector" Core..=)
              Prelude.<$> colorCorrector,
            ("timecodeBurnin" Core..=)
              Prelude.<$> timecodeBurnin,
            ("hdr10Plus" Core..=) Prelude.<$> hdr10Plus,
            ("imageInserter" Core..=) Prelude.<$> imageInserter,
            ("deinterlacer" Core..=) Prelude.<$> deinterlacer,
            ("noiseReducer" Core..=) Prelude.<$> noiseReducer
          ]
      )
