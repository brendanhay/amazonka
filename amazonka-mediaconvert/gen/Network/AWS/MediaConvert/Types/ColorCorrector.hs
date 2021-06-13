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
import qualified Network.AWS.Prelude as Prelude

-- | Settings for color correction.
--
-- /See:/ 'newColorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { -- | Saturation level.
    saturation :: Prelude.Maybe Prelude.Natural,
    -- | Specify the color space you want for this output. The service supports
    -- conversion between HDR formats, between SDR formats, from SDR to HDR,
    -- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
    -- range. The converted video has an HDR format, but visually appears the
    -- same as an unconverted output. HDR to SDR conversion uses Elemental tone
    -- mapping technology to approximate the outcome of manually regrading from
    -- HDR to SDR.
    colorSpaceConversion :: Prelude.Maybe ColorSpaceConversion,
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
    -- | Brightness level.
    brightness :: Prelude.Maybe Prelude.Natural,
    -- | Hue in degrees.
    hue :: Prelude.Maybe Prelude.Int,
    -- | Contrast level.
    contrast :: Prelude.Maybe Prelude.Natural
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
    { saturation = Prelude.Nothing,
      colorSpaceConversion = Prelude.Nothing,
      hdr10Metadata = Prelude.Nothing,
      brightness = Prelude.Nothing,
      hue = Prelude.Nothing,
      contrast = Prelude.Nothing
    }

-- | Saturation level.
colorCorrector_saturation :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_saturation = Lens.lens (\ColorCorrector' {saturation} -> saturation) (\s@ColorCorrector' {} a -> s {saturation = a} :: ColorCorrector)

-- | Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses Elemental tone
-- mapping technology to approximate the outcome of manually regrading from
-- HDR to SDR.
colorCorrector_colorSpaceConversion :: Lens.Lens' ColorCorrector (Prelude.Maybe ColorSpaceConversion)
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
colorCorrector_hdr10Metadata :: Lens.Lens' ColorCorrector (Prelude.Maybe Hdr10Metadata)
colorCorrector_hdr10Metadata = Lens.lens (\ColorCorrector' {hdr10Metadata} -> hdr10Metadata) (\s@ColorCorrector' {} a -> s {hdr10Metadata = a} :: ColorCorrector)

-- | Brightness level.
colorCorrector_brightness :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_brightness = Lens.lens (\ColorCorrector' {brightness} -> brightness) (\s@ColorCorrector' {} a -> s {brightness = a} :: ColorCorrector)

-- | Hue in degrees.
colorCorrector_hue :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Int)
colorCorrector_hue = Lens.lens (\ColorCorrector' {hue} -> hue) (\s@ColorCorrector' {} a -> s {hue = a} :: ColorCorrector)

-- | Contrast level.
colorCorrector_contrast :: Lens.Lens' ColorCorrector (Prelude.Maybe Prelude.Natural)
colorCorrector_contrast = Lens.lens (\ColorCorrector' {contrast} -> contrast) (\s@ColorCorrector' {} a -> s {contrast = a} :: ColorCorrector)

instance Core.FromJSON ColorCorrector where
  parseJSON =
    Core.withObject
      "ColorCorrector"
      ( \x ->
          ColorCorrector'
            Prelude.<$> (x Core..:? "saturation")
            Prelude.<*> (x Core..:? "colorSpaceConversion")
            Prelude.<*> (x Core..:? "hdr10Metadata")
            Prelude.<*> (x Core..:? "brightness")
            Prelude.<*> (x Core..:? "hue")
            Prelude.<*> (x Core..:? "contrast")
      )

instance Prelude.Hashable ColorCorrector

instance Prelude.NFData ColorCorrector

instance Core.ToJSON ColorCorrector where
  toJSON ColorCorrector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("saturation" Core..=) Prelude.<$> saturation,
            ("colorSpaceConversion" Core..=)
              Prelude.<$> colorSpaceConversion,
            ("hdr10Metadata" Core..=) Prelude.<$> hdr10Metadata,
            ("brightness" Core..=) Prelude.<$> brightness,
            ("hue" Core..=) Prelude.<$> hue,
            ("contrast" Core..=) Prelude.<$> contrast
          ]
      )
