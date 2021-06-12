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
-- Module      : Network.AWS.MediaConvert.Types.ColorCorrector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorCorrector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ColorSpaceConversion
import Network.AWS.MediaConvert.Types.Hdr10Metadata

-- | Settings for color correction.
--
-- /See:/ 'newColorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { -- | Saturation level.
    saturation :: Core.Maybe Core.Natural,
    -- | Specify the color space you want for this output. The service supports
    -- conversion between HDR formats, between SDR formats, from SDR to HDR,
    -- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
    -- range. The converted video has an HDR format, but visually appears the
    -- same as an unconverted output. HDR to SDR conversion uses Elemental tone
    -- mapping technology to approximate the outcome of manually regrading from
    -- HDR to SDR.
    colorSpaceConversion :: Core.Maybe ColorSpaceConversion,
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
    hdr10Metadata :: Core.Maybe Hdr10Metadata,
    -- | Brightness level.
    brightness :: Core.Maybe Core.Natural,
    -- | Hue in degrees.
    hue :: Core.Maybe Core.Int,
    -- | Contrast level.
    contrast :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ColorCorrector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'saturation', 'colorCorrector_saturation' - Saturation level.
--
-- 'colorSpaceConversion', 'colorCorrector_colorSpaceConversion' - Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses Elemental tone
-- mapping technology to approximate the outcome of manually regrading from
-- HDR to SDR.
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
-- 'brightness', 'colorCorrector_brightness' - Brightness level.
--
-- 'hue', 'colorCorrector_hue' - Hue in degrees.
--
-- 'contrast', 'colorCorrector_contrast' - Contrast level.
newColorCorrector ::
  ColorCorrector
newColorCorrector =
  ColorCorrector'
    { saturation = Core.Nothing,
      colorSpaceConversion = Core.Nothing,
      hdr10Metadata = Core.Nothing,
      brightness = Core.Nothing,
      hue = Core.Nothing,
      contrast = Core.Nothing
    }

-- | Saturation level.
colorCorrector_saturation :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
colorCorrector_saturation = Lens.lens (\ColorCorrector' {saturation} -> saturation) (\s@ColorCorrector' {} a -> s {saturation = a} :: ColorCorrector)

-- | Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses Elemental tone
-- mapping technology to approximate the outcome of manually regrading from
-- HDR to SDR.
colorCorrector_colorSpaceConversion :: Lens.Lens' ColorCorrector (Core.Maybe ColorSpaceConversion)
colorCorrector_colorSpaceConversion = Lens.lens (\ColorCorrector' {colorSpaceConversion} -> colorSpaceConversion) (\s@ColorCorrector' {} a -> s {colorSpaceConversion = a} :: ColorCorrector)

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
colorCorrector_hdr10Metadata :: Lens.Lens' ColorCorrector (Core.Maybe Hdr10Metadata)
colorCorrector_hdr10Metadata = Lens.lens (\ColorCorrector' {hdr10Metadata} -> hdr10Metadata) (\s@ColorCorrector' {} a -> s {hdr10Metadata = a} :: ColorCorrector)

-- | Brightness level.
colorCorrector_brightness :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
colorCorrector_brightness = Lens.lens (\ColorCorrector' {brightness} -> brightness) (\s@ColorCorrector' {} a -> s {brightness = a} :: ColorCorrector)

-- | Hue in degrees.
colorCorrector_hue :: Lens.Lens' ColorCorrector (Core.Maybe Core.Int)
colorCorrector_hue = Lens.lens (\ColorCorrector' {hue} -> hue) (\s@ColorCorrector' {} a -> s {hue = a} :: ColorCorrector)

-- | Contrast level.
colorCorrector_contrast :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
colorCorrector_contrast = Lens.lens (\ColorCorrector' {contrast} -> contrast) (\s@ColorCorrector' {} a -> s {contrast = a} :: ColorCorrector)

instance Core.FromJSON ColorCorrector where
  parseJSON =
    Core.withObject
      "ColorCorrector"
      ( \x ->
          ColorCorrector'
            Core.<$> (x Core..:? "saturation")
            Core.<*> (x Core..:? "colorSpaceConversion")
            Core.<*> (x Core..:? "hdr10Metadata")
            Core.<*> (x Core..:? "brightness")
            Core.<*> (x Core..:? "hue")
            Core.<*> (x Core..:? "contrast")
      )

instance Core.Hashable ColorCorrector

instance Core.NFData ColorCorrector

instance Core.ToJSON ColorCorrector where
  toJSON ColorCorrector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("saturation" Core..=) Core.<$> saturation,
            ("colorSpaceConversion" Core..=)
              Core.<$> colorSpaceConversion,
            ("hdr10Metadata" Core..=) Core.<$> hdr10Metadata,
            ("brightness" Core..=) Core.<$> brightness,
            ("hue" Core..=) Core.<$> hue,
            ("contrast" Core..=) Core.<$> contrast
          ]
      )
