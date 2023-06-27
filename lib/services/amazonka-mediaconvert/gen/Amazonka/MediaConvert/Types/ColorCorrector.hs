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
-- Module      : Amazonka.MediaConvert.Types.ColorCorrector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ColorCorrector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.ClipLimits
import Amazonka.MediaConvert.Types.ColorSpaceConversion
import Amazonka.MediaConvert.Types.HDRToSDRToneMapper
import Amazonka.MediaConvert.Types.Hdr10Metadata
import Amazonka.MediaConvert.Types.SampleRangeConversion
import qualified Amazonka.Prelude as Prelude

-- | Settings for color correction.
--
-- /See:/ 'newColorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { -- | Brightness level.
    brightness :: Prelude.Maybe Prelude.Natural,
    -- | Specify YUV limits and RGB tolerances when you set Sample range
    -- conversion to Limited range clip.
    clipLimits :: Prelude.Maybe ClipLimits,
    -- | Specify the color space you want for this output. The service supports
    -- conversion between HDR formats, between SDR formats, from SDR to HDR,
    -- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
    -- range. The converted video has an HDR format, but visually appears the
    -- same as an unconverted output. HDR to SDR conversion uses tone mapping
    -- to approximate the outcome of manually regrading from HDR to SDR. When
    -- you specify an output color space, MediaConvert uses the following color
    -- space metadata, which includes color primaries, transfer
    -- characteristics, and matrix coefficients: * HDR 10: BT.2020, PQ, BT.2020
    -- non-constant * HLG 2020: BT.2020, HLG, BT.2020 non-constant * P3DCI
    -- (Theater): DCIP3, SMPTE 428M, BT.709 * P3D65 (SDR): Display P3, sRGB,
    -- BT.709 * P3D65 (HDR): Display P3, PQ, BT.709
    colorSpaceConversion :: Prelude.Maybe ColorSpaceConversion,
    -- | Contrast level.
    contrast :: Prelude.Maybe Prelude.Natural,
    -- | Use these settings when you convert to the HDR 10 color space. Specify
    -- the SMPTE ST 2086 Mastering Display Color Volume static metadata that
    -- you want signaled in the output. These values don\'t affect the pixel
    -- values that are encoded in the video stream. They are intended to help
    -- the downstream video player display content in a way that reflects the
    -- intentions of the the content creator. When you set Color space
    -- conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these
    -- settings are required. You must set values for Max frame average light
    -- level (maxFrameAverageLightLevel) and Max content light level
    -- (maxContentLightLevel); these settings don\'t have a default value. The
    -- default values for the other HDR 10 metadata settings are defined by the
    -- P3D65 color space. For more information about MediaConvert HDR jobs, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
    hdr10Metadata :: Prelude.Maybe Hdr10Metadata,
    -- | Specify how MediaConvert maps brightness and colors from your HDR input
    -- to your SDR output. The mode that you select represents a creative
    -- choice, with different tradeoffs in the details and tones of your
    -- output. To maintain details in bright or saturated areas of your output:
    -- Choose Preserve details. For some sources, your SDR output may look less
    -- bright and less saturated when compared to your HDR source. MediaConvert
    -- automatically applies this mode for HLG sources, regardless of your
    -- choice. For a bright and saturated output: Choose Vibrant. We recommend
    -- that you choose this mode when any of your source content is HDR10, and
    -- for the best results when it is mastered for 1000 nits. You may notice
    -- loss of details in bright or saturated areas of your output. HDR to SDR
    -- tone mapping has no effect when your input is SDR.
    hdrToSdrToneMapper :: Prelude.Maybe HDRToSDRToneMapper,
    -- | Hue in degrees.
    hue :: Prelude.Maybe Prelude.Int,
    -- | Specify how MediaConvert limits the color sample range for this output.
    -- To create a limited range output from a full range input: Choose Limited
    -- range squeeze. For full range inputs, MediaConvert performs a linear
    -- offset to color samples equally across all pixels and frames. Color
    -- samples in 10-bit outputs are limited to 64 through 940, and 8-bit
    -- outputs are limited to 16 through 235. Note: For limited range inputs,
    -- values for color samples are passed through to your output unchanged.
    -- MediaConvert does not limit the sample range. To correct pixels in your
    -- input that are out of range or out of gamut: Choose Limited range clip.
    -- Use for broadcast applications. MediaConvert conforms any pixels outside
    -- of the values that you specify under Minimum YUV and Maximum YUV to
    -- limited range bounds. MediaConvert also corrects any YUV values that,
    -- when converted to RGB, would be outside the bounds you specify under
    -- Minimum RGB tolerance and Maximum RGB tolerance. With either limited
    -- range conversion, MediaConvert writes the sample range metadata in the
    -- output.
    sampleRangeConversion :: Prelude.Maybe SampleRangeConversion,
    -- | Saturation level.
    saturation :: Prelude.Maybe Prelude.Natural,
    -- | Specify the reference white level, in nits, for all of your SDR inputs.
    -- Use to correct brightness levels within HDR10 outputs. The following
    -- color metadata must be present in your SDR input: color primaries,
    -- transfer characteristics, and matrix coefficients. If your SDR input has
    -- missing color metadata, or if you want to correct input color metadata,
    -- manually specify a color space in the input video selector. For 1,000
    -- nit peak brightness displays, we recommend that you set SDR reference
    -- white level to 203 (according to ITU-R BT.2408). Leave blank to use the
    -- default value of 100, or specify an integer from 100 to 1000.
    sdrReferenceWhiteLevel :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColorCorrector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brightness', 'colorCorrector_brightness' - Brightness level.
--
-- 'clipLimits', 'colorCorrector_clipLimits' - Specify YUV limits and RGB tolerances when you set Sample range
-- conversion to Limited range clip.
--
-- 'colorSpaceConversion', 'colorCorrector_colorSpaceConversion' - Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses tone mapping
-- to approximate the outcome of manually regrading from HDR to SDR. When
-- you specify an output color space, MediaConvert uses the following color
-- space metadata, which includes color primaries, transfer
-- characteristics, and matrix coefficients: * HDR 10: BT.2020, PQ, BT.2020
-- non-constant * HLG 2020: BT.2020, HLG, BT.2020 non-constant * P3DCI
-- (Theater): DCIP3, SMPTE 428M, BT.709 * P3D65 (SDR): Display P3, sRGB,
-- BT.709 * P3D65 (HDR): Display P3, PQ, BT.709
--
-- 'contrast', 'colorCorrector_contrast' - Contrast level.
--
-- 'hdr10Metadata', 'colorCorrector_hdr10Metadata' - Use these settings when you convert to the HDR 10 color space. Specify
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata that
-- you want signaled in the output. These values don\'t affect the pixel
-- values that are encoded in the video stream. They are intended to help
-- the downstream video player display content in a way that reflects the
-- intentions of the the content creator. When you set Color space
-- conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these
-- settings are required. You must set values for Max frame average light
-- level (maxFrameAverageLightLevel) and Max content light level
-- (maxContentLightLevel); these settings don\'t have a default value. The
-- default values for the other HDR 10 metadata settings are defined by the
-- P3D65 color space. For more information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
--
-- 'hdrToSdrToneMapper', 'colorCorrector_hdrToSdrToneMapper' - Specify how MediaConvert maps brightness and colors from your HDR input
-- to your SDR output. The mode that you select represents a creative
-- choice, with different tradeoffs in the details and tones of your
-- output. To maintain details in bright or saturated areas of your output:
-- Choose Preserve details. For some sources, your SDR output may look less
-- bright and less saturated when compared to your HDR source. MediaConvert
-- automatically applies this mode for HLG sources, regardless of your
-- choice. For a bright and saturated output: Choose Vibrant. We recommend
-- that you choose this mode when any of your source content is HDR10, and
-- for the best results when it is mastered for 1000 nits. You may notice
-- loss of details in bright or saturated areas of your output. HDR to SDR
-- tone mapping has no effect when your input is SDR.
--
-- 'hue', 'colorCorrector_hue' - Hue in degrees.
--
-- 'sampleRangeConversion', 'colorCorrector_sampleRangeConversion' - Specify how MediaConvert limits the color sample range for this output.
-- To create a limited range output from a full range input: Choose Limited
-- range squeeze. For full range inputs, MediaConvert performs a linear
-- offset to color samples equally across all pixels and frames. Color
-- samples in 10-bit outputs are limited to 64 through 940, and 8-bit
-- outputs are limited to 16 through 235. Note: For limited range inputs,
-- values for color samples are passed through to your output unchanged.
-- MediaConvert does not limit the sample range. To correct pixels in your
-- input that are out of range or out of gamut: Choose Limited range clip.
-- Use for broadcast applications. MediaConvert conforms any pixels outside
-- of the values that you specify under Minimum YUV and Maximum YUV to
-- limited range bounds. MediaConvert also corrects any YUV values that,
-- when converted to RGB, would be outside the bounds you specify under
-- Minimum RGB tolerance and Maximum RGB tolerance. With either limited
-- range conversion, MediaConvert writes the sample range metadata in the
-- output.
--
-- 'saturation', 'colorCorrector_saturation' - Saturation level.
--
-- 'sdrReferenceWhiteLevel', 'colorCorrector_sdrReferenceWhiteLevel' - Specify the reference white level, in nits, for all of your SDR inputs.
-- Use to correct brightness levels within HDR10 outputs. The following
-- color metadata must be present in your SDR input: color primaries,
-- transfer characteristics, and matrix coefficients. If your SDR input has
-- missing color metadata, or if you want to correct input color metadata,
-- manually specify a color space in the input video selector. For 1,000
-- nit peak brightness displays, we recommend that you set SDR reference
-- white level to 203 (according to ITU-R BT.2408). Leave blank to use the
-- default value of 100, or specify an integer from 100 to 1000.
newColorCorrector ::
  ColorCorrector
newColorCorrector =
  ColorCorrector'
    { brightness = Prelude.Nothing,
      clipLimits = Prelude.Nothing,
      colorSpaceConversion = Prelude.Nothing,
      contrast = Prelude.Nothing,
      hdr10Metadata = Prelude.Nothing,
      hdrToSdrToneMapper = Prelude.Nothing,
      hue = Prelude.Nothing,
      sampleRangeConversion = Prelude.Nothing,
      saturation = Prelude.Nothing,
      sdrReferenceWhiteLevel = Prelude.Nothing
    }

-- | Brightness level.
colorCorrector_brightness :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_brightness = Lens.lens (\ColorCorrector' {brightness} -> brightness) (\s@ColorCorrector' {} a -> s {brightness = a} :: ColorCorrector)

-- | Specify YUV limits and RGB tolerances when you set Sample range
-- conversion to Limited range clip.
colorCorrector_clipLimits :: Lens.Lens' ColorCorrector (Prelude.Maybe ClipLimits)
colorCorrector_clipLimits = Lens.lens (\ColorCorrector' {clipLimits} -> clipLimits) (\s@ColorCorrector' {} a -> s {clipLimits = a} :: ColorCorrector)

-- | Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses tone mapping
-- to approximate the outcome of manually regrading from HDR to SDR. When
-- you specify an output color space, MediaConvert uses the following color
-- space metadata, which includes color primaries, transfer
-- characteristics, and matrix coefficients: * HDR 10: BT.2020, PQ, BT.2020
-- non-constant * HLG 2020: BT.2020, HLG, BT.2020 non-constant * P3DCI
-- (Theater): DCIP3, SMPTE 428M, BT.709 * P3D65 (SDR): Display P3, sRGB,
-- BT.709 * P3D65 (HDR): Display P3, PQ, BT.709
colorCorrector_colorSpaceConversion :: Lens.Lens' ColorCorrector (Prelude.Maybe ColorSpaceConversion)
colorCorrector_colorSpaceConversion = Lens.lens (\ColorCorrector' {colorSpaceConversion} -> colorSpaceConversion) (\s@ColorCorrector' {} a -> s {colorSpaceConversion = a} :: ColorCorrector)

-- | Contrast level.
colorCorrector_contrast :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_contrast = Lens.lens (\ColorCorrector' {contrast} -> contrast) (\s@ColorCorrector' {} a -> s {contrast = a} :: ColorCorrector)

-- | Use these settings when you convert to the HDR 10 color space. Specify
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata that
-- you want signaled in the output. These values don\'t affect the pixel
-- values that are encoded in the video stream. They are intended to help
-- the downstream video player display content in a way that reflects the
-- intentions of the the content creator. When you set Color space
-- conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these
-- settings are required. You must set values for Max frame average light
-- level (maxFrameAverageLightLevel) and Max content light level
-- (maxContentLightLevel); these settings don\'t have a default value. The
-- default values for the other HDR 10 metadata settings are defined by the
-- P3D65 color space. For more information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
colorCorrector_hdr10Metadata :: Lens.Lens' ColorCorrector (Prelude.Maybe Hdr10Metadata)
colorCorrector_hdr10Metadata = Lens.lens (\ColorCorrector' {hdr10Metadata} -> hdr10Metadata) (\s@ColorCorrector' {} a -> s {hdr10Metadata = a} :: ColorCorrector)

-- | Specify how MediaConvert maps brightness and colors from your HDR input
-- to your SDR output. The mode that you select represents a creative
-- choice, with different tradeoffs in the details and tones of your
-- output. To maintain details in bright or saturated areas of your output:
-- Choose Preserve details. For some sources, your SDR output may look less
-- bright and less saturated when compared to your HDR source. MediaConvert
-- automatically applies this mode for HLG sources, regardless of your
-- choice. For a bright and saturated output: Choose Vibrant. We recommend
-- that you choose this mode when any of your source content is HDR10, and
-- for the best results when it is mastered for 1000 nits. You may notice
-- loss of details in bright or saturated areas of your output. HDR to SDR
-- tone mapping has no effect when your input is SDR.
colorCorrector_hdrToSdrToneMapper :: Lens.Lens' ColorCorrector (Prelude.Maybe HDRToSDRToneMapper)
colorCorrector_hdrToSdrToneMapper = Lens.lens (\ColorCorrector' {hdrToSdrToneMapper} -> hdrToSdrToneMapper) (\s@ColorCorrector' {} a -> s {hdrToSdrToneMapper = a} :: ColorCorrector)

-- | Hue in degrees.
colorCorrector_hue :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Int)
colorCorrector_hue = Lens.lens (\ColorCorrector' {hue} -> hue) (\s@ColorCorrector' {} a -> s {hue = a} :: ColorCorrector)

-- | Specify how MediaConvert limits the color sample range for this output.
-- To create a limited range output from a full range input: Choose Limited
-- range squeeze. For full range inputs, MediaConvert performs a linear
-- offset to color samples equally across all pixels and frames. Color
-- samples in 10-bit outputs are limited to 64 through 940, and 8-bit
-- outputs are limited to 16 through 235. Note: For limited range inputs,
-- values for color samples are passed through to your output unchanged.
-- MediaConvert does not limit the sample range. To correct pixels in your
-- input that are out of range or out of gamut: Choose Limited range clip.
-- Use for broadcast applications. MediaConvert conforms any pixels outside
-- of the values that you specify under Minimum YUV and Maximum YUV to
-- limited range bounds. MediaConvert also corrects any YUV values that,
-- when converted to RGB, would be outside the bounds you specify under
-- Minimum RGB tolerance and Maximum RGB tolerance. With either limited
-- range conversion, MediaConvert writes the sample range metadata in the
-- output.
colorCorrector_sampleRangeConversion :: Lens.Lens' ColorCorrector (Prelude.Maybe SampleRangeConversion)
colorCorrector_sampleRangeConversion = Lens.lens (\ColorCorrector' {sampleRangeConversion} -> sampleRangeConversion) (\s@ColorCorrector' {} a -> s {sampleRangeConversion = a} :: ColorCorrector)

-- | Saturation level.
colorCorrector_saturation :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_saturation = Lens.lens (\ColorCorrector' {saturation} -> saturation) (\s@ColorCorrector' {} a -> s {saturation = a} :: ColorCorrector)

-- | Specify the reference white level, in nits, for all of your SDR inputs.
-- Use to correct brightness levels within HDR10 outputs. The following
-- color metadata must be present in your SDR input: color primaries,
-- transfer characteristics, and matrix coefficients. If your SDR input has
-- missing color metadata, or if you want to correct input color metadata,
-- manually specify a color space in the input video selector. For 1,000
-- nit peak brightness displays, we recommend that you set SDR reference
-- white level to 203 (according to ITU-R BT.2408). Leave blank to use the
-- default value of 100, or specify an integer from 100 to 1000.
colorCorrector_sdrReferenceWhiteLevel :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_sdrReferenceWhiteLevel = Lens.lens (\ColorCorrector' {sdrReferenceWhiteLevel} -> sdrReferenceWhiteLevel) (\s@ColorCorrector' {} a -> s {sdrReferenceWhiteLevel = a} :: ColorCorrector)

instance Data.FromJSON ColorCorrector where
  parseJSON =
    Data.withObject
      "ColorCorrector"
      ( \x ->
          ColorCorrector'
            Prelude.<$> (x Data..:? "brightness")
            Prelude.<*> (x Data..:? "clipLimits")
            Prelude.<*> (x Data..:? "colorSpaceConversion")
            Prelude.<*> (x Data..:? "contrast")
            Prelude.<*> (x Data..:? "hdr10Metadata")
            Prelude.<*> (x Data..:? "hdrToSdrToneMapper")
            Prelude.<*> (x Data..:? "hue")
            Prelude.<*> (x Data..:? "sampleRangeConversion")
            Prelude.<*> (x Data..:? "saturation")
            Prelude.<*> (x Data..:? "sdrReferenceWhiteLevel")
      )

instance Prelude.Hashable ColorCorrector where
  hashWithSalt _salt ColorCorrector' {..} =
    _salt
      `Prelude.hashWithSalt` brightness
      `Prelude.hashWithSalt` clipLimits
      `Prelude.hashWithSalt` colorSpaceConversion
      `Prelude.hashWithSalt` contrast
      `Prelude.hashWithSalt` hdr10Metadata
      `Prelude.hashWithSalt` hdrToSdrToneMapper
      `Prelude.hashWithSalt` hue
      `Prelude.hashWithSalt` sampleRangeConversion
      `Prelude.hashWithSalt` saturation
      `Prelude.hashWithSalt` sdrReferenceWhiteLevel

instance Prelude.NFData ColorCorrector where
  rnf ColorCorrector' {..} =
    Prelude.rnf brightness
      `Prelude.seq` Prelude.rnf clipLimits
      `Prelude.seq` Prelude.rnf colorSpaceConversion
      `Prelude.seq` Prelude.rnf contrast
      `Prelude.seq` Prelude.rnf hdr10Metadata
      `Prelude.seq` Prelude.rnf hdrToSdrToneMapper
      `Prelude.seq` Prelude.rnf hue
      `Prelude.seq` Prelude.rnf sampleRangeConversion
      `Prelude.seq` Prelude.rnf saturation
      `Prelude.seq` Prelude.rnf sdrReferenceWhiteLevel

instance Data.ToJSON ColorCorrector where
  toJSON ColorCorrector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("brightness" Data..=) Prelude.<$> brightness,
            ("clipLimits" Data..=) Prelude.<$> clipLimits,
            ("colorSpaceConversion" Data..=)
              Prelude.<$> colorSpaceConversion,
            ("contrast" Data..=) Prelude.<$> contrast,
            ("hdr10Metadata" Data..=) Prelude.<$> hdr10Metadata,
            ("hdrToSdrToneMapper" Data..=)
              Prelude.<$> hdrToSdrToneMapper,
            ("hue" Data..=) Prelude.<$> hue,
            ("sampleRangeConversion" Data..=)
              Prelude.<$> sampleRangeConversion,
            ("saturation" Data..=) Prelude.<$> saturation,
            ("sdrReferenceWhiteLevel" Data..=)
              Prelude.<$> sdrReferenceWhiteLevel
          ]
      )
