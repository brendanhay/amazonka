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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoPreprocessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Use these settings to convert the color space or to modify properties
    -- such as hue and contrast for this output. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
    colorCorrector :: Prelude.Maybe ColorCorrector,
    -- | Use the deinterlacer to produce smoother motion and a clearer picture.
    -- For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
    deinterlacer :: Prelude.Maybe Deinterlacer,
    -- | Enable Dolby Vision feature to produce Dolby Vision compatible video
    -- output.
    dolbyVision :: Prelude.Maybe DolbyVision,
    -- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
    hdr10Plus :: Prelude.Maybe Hdr10Plus,
    -- | Enable the Image inserter (ImageInserter) feature to include a graphic
    -- overlay on your video. Enable or disable this feature for each output
    -- individually. This setting is disabled by default.
    imageInserter :: Prelude.Maybe ImageInserter,
    -- | Enable the Noise reducer feature to remove noise from your video output
    -- if necessary. Enable or disable this feature for each output
    -- individually. This setting is disabled by default. When you enable Noise
    -- reducer, you must also select a value for Noise reducer filter. For AVC
    -- outputs, when you include Noise reducer, you cannot include the
    -- Bandwidth reduction filter.
    noiseReducer :: Prelude.Maybe NoiseReducer,
    -- | If you work with a third party video watermarking partner, use the group
    -- of settings that correspond with your watermarking partner to include
    -- watermarks in your output.
    partnerWatermarking :: Prelude.Maybe PartnerWatermarking,
    -- | Settings for burning the output timecode and specified prefix into the
    -- output.
    timecodeBurnin :: Prelude.Maybe TimecodeBurnin
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
-- 'colorCorrector', 'videoPreprocessor_colorCorrector' - Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
--
-- 'deinterlacer', 'videoPreprocessor_deinterlacer' - Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
--
-- 'dolbyVision', 'videoPreprocessor_dolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
--
-- 'hdr10Plus', 'videoPreprocessor_hdr10Plus' - Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
--
-- 'imageInserter', 'videoPreprocessor_imageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
--
-- 'noiseReducer', 'videoPreprocessor_noiseReducer' - Enable the Noise reducer feature to remove noise from your video output
-- if necessary. Enable or disable this feature for each output
-- individually. This setting is disabled by default. When you enable Noise
-- reducer, you must also select a value for Noise reducer filter. For AVC
-- outputs, when you include Noise reducer, you cannot include the
-- Bandwidth reduction filter.
--
-- 'partnerWatermarking', 'videoPreprocessor_partnerWatermarking' - If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
--
-- 'timecodeBurnin', 'videoPreprocessor_timecodeBurnin' - Settings for burning the output timecode and specified prefix into the
-- output.
newVideoPreprocessor ::
  VideoPreprocessor
newVideoPreprocessor =
  VideoPreprocessor'
    { colorCorrector =
        Prelude.Nothing,
      deinterlacer = Prelude.Nothing,
      dolbyVision = Prelude.Nothing,
      hdr10Plus = Prelude.Nothing,
      imageInserter = Prelude.Nothing,
      noiseReducer = Prelude.Nothing,
      partnerWatermarking = Prelude.Nothing,
      timecodeBurnin = Prelude.Nothing
    }

-- | Use these settings to convert the color space or to modify properties
-- such as hue and contrast for this output. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/converting-the-color-space.html.
videoPreprocessor_colorCorrector :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ColorCorrector)
videoPreprocessor_colorCorrector = Lens.lens (\VideoPreprocessor' {colorCorrector} -> colorCorrector) (\s@VideoPreprocessor' {} a -> s {colorCorrector = a} :: VideoPreprocessor)

-- | Use the deinterlacer to produce smoother motion and a clearer picture.
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-scan-type.html.
videoPreprocessor_deinterlacer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Deinterlacer)
videoPreprocessor_deinterlacer = Lens.lens (\VideoPreprocessor' {deinterlacer} -> deinterlacer) (\s@VideoPreprocessor' {} a -> s {deinterlacer = a} :: VideoPreprocessor)

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video
-- output.
videoPreprocessor_dolbyVision :: Lens.Lens' VideoPreprocessor (Prelude.Maybe DolbyVision)
videoPreprocessor_dolbyVision = Lens.lens (\VideoPreprocessor' {dolbyVision} -> dolbyVision) (\s@VideoPreprocessor' {} a -> s {dolbyVision = a} :: VideoPreprocessor)

-- | Enable HDR10+ analyis and metadata injection. Compatible with HEVC only.
videoPreprocessor_hdr10Plus :: Lens.Lens' VideoPreprocessor (Prelude.Maybe Hdr10Plus)
videoPreprocessor_hdr10Plus = Lens.lens (\VideoPreprocessor' {hdr10Plus} -> hdr10Plus) (\s@VideoPreprocessor' {} a -> s {hdr10Plus = a} :: VideoPreprocessor)

-- | Enable the Image inserter (ImageInserter) feature to include a graphic
-- overlay on your video. Enable or disable this feature for each output
-- individually. This setting is disabled by default.
videoPreprocessor_imageInserter :: Lens.Lens' VideoPreprocessor (Prelude.Maybe ImageInserter)
videoPreprocessor_imageInserter = Lens.lens (\VideoPreprocessor' {imageInserter} -> imageInserter) (\s@VideoPreprocessor' {} a -> s {imageInserter = a} :: VideoPreprocessor)

-- | Enable the Noise reducer feature to remove noise from your video output
-- if necessary. Enable or disable this feature for each output
-- individually. This setting is disabled by default. When you enable Noise
-- reducer, you must also select a value for Noise reducer filter. For AVC
-- outputs, when you include Noise reducer, you cannot include the
-- Bandwidth reduction filter.
videoPreprocessor_noiseReducer :: Lens.Lens' VideoPreprocessor (Prelude.Maybe NoiseReducer)
videoPreprocessor_noiseReducer = Lens.lens (\VideoPreprocessor' {noiseReducer} -> noiseReducer) (\s@VideoPreprocessor' {} a -> s {noiseReducer = a} :: VideoPreprocessor)

-- | If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
videoPreprocessor_partnerWatermarking :: Lens.Lens' VideoPreprocessor (Prelude.Maybe PartnerWatermarking)
videoPreprocessor_partnerWatermarking = Lens.lens (\VideoPreprocessor' {partnerWatermarking} -> partnerWatermarking) (\s@VideoPreprocessor' {} a -> s {partnerWatermarking = a} :: VideoPreprocessor)

-- | Settings for burning the output timecode and specified prefix into the
-- output.
videoPreprocessor_timecodeBurnin :: Lens.Lens' VideoPreprocessor (Prelude.Maybe TimecodeBurnin)
videoPreprocessor_timecodeBurnin = Lens.lens (\VideoPreprocessor' {timecodeBurnin} -> timecodeBurnin) (\s@VideoPreprocessor' {} a -> s {timecodeBurnin = a} :: VideoPreprocessor)

instance Data.FromJSON VideoPreprocessor where
  parseJSON =
    Data.withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            Prelude.<$> (x Data..:? "colorCorrector")
            Prelude.<*> (x Data..:? "deinterlacer")
            Prelude.<*> (x Data..:? "dolbyVision")
            Prelude.<*> (x Data..:? "hdr10Plus")
            Prelude.<*> (x Data..:? "imageInserter")
            Prelude.<*> (x Data..:? "noiseReducer")
            Prelude.<*> (x Data..:? "partnerWatermarking")
            Prelude.<*> (x Data..:? "timecodeBurnin")
      )

instance Prelude.Hashable VideoPreprocessor where
  hashWithSalt _salt VideoPreprocessor' {..} =
    _salt
      `Prelude.hashWithSalt` colorCorrector
      `Prelude.hashWithSalt` deinterlacer
      `Prelude.hashWithSalt` dolbyVision
      `Prelude.hashWithSalt` hdr10Plus
      `Prelude.hashWithSalt` imageInserter
      `Prelude.hashWithSalt` noiseReducer
      `Prelude.hashWithSalt` partnerWatermarking
      `Prelude.hashWithSalt` timecodeBurnin

instance Prelude.NFData VideoPreprocessor where
  rnf VideoPreprocessor' {..} =
    Prelude.rnf colorCorrector
      `Prelude.seq` Prelude.rnf deinterlacer
      `Prelude.seq` Prelude.rnf dolbyVision
      `Prelude.seq` Prelude.rnf hdr10Plus
      `Prelude.seq` Prelude.rnf imageInserter
      `Prelude.seq` Prelude.rnf noiseReducer
      `Prelude.seq` Prelude.rnf partnerWatermarking
      `Prelude.seq` Prelude.rnf timecodeBurnin

instance Data.ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("colorCorrector" Data..=)
              Prelude.<$> colorCorrector,
            ("deinterlacer" Data..=) Prelude.<$> deinterlacer,
            ("dolbyVision" Data..=) Prelude.<$> dolbyVision,
            ("hdr10Plus" Data..=) Prelude.<$> hdr10Plus,
            ("imageInserter" Data..=) Prelude.<$> imageInserter,
            ("noiseReducer" Data..=) Prelude.<$> noiseReducer,
            ("partnerWatermarking" Data..=)
              Prelude.<$> partnerWatermarking,
            ("timecodeBurnin" Data..=)
              Prelude.<$> timecodeBurnin
          ]
      )
