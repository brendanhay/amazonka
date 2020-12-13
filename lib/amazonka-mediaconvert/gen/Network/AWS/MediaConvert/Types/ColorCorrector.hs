{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorCorrector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorCorrector
  ( ColorCorrector (..),

    -- * Smart constructor
    mkColorCorrector,

    -- * Lenses
    ccSaturation,
    ccHue,
    ccColorSpaceConversion,
    ccHdr10Metadata,
    ccContrast,
    ccBrightness,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ColorSpaceConversion
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import qualified Network.AWS.Prelude as Lude

-- | Settings for color correction.
--
-- /See:/ 'mkColorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { -- | Saturation level.
    saturation :: Lude.Maybe Lude.Natural,
    -- | Hue in degrees.
    hue :: Lude.Maybe Lude.Int,
    -- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
    colorSpaceConversion :: Lude.Maybe ColorSpaceConversion,
    -- | Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
    hdr10Metadata :: Lude.Maybe Hdr10Metadata,
    -- | Contrast level.
    contrast :: Lude.Maybe Lude.Natural,
    -- | Brightness level.
    brightness :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColorCorrector' with the minimum fields required to make a request.
--
-- * 'saturation' - Saturation level.
-- * 'hue' - Hue in degrees.
-- * 'colorSpaceConversion' - Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
-- * 'hdr10Metadata' - Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
-- * 'contrast' - Contrast level.
-- * 'brightness' - Brightness level.
mkColorCorrector ::
  ColorCorrector
mkColorCorrector =
  ColorCorrector'
    { saturation = Lude.Nothing,
      hue = Lude.Nothing,
      colorSpaceConversion = Lude.Nothing,
      hdr10Metadata = Lude.Nothing,
      contrast = Lude.Nothing,
      brightness = Lude.Nothing
    }

-- | Saturation level.
--
-- /Note:/ Consider using 'saturation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSaturation :: Lens.Lens' ColorCorrector (Lude.Maybe Lude.Natural)
ccSaturation = Lens.lens (saturation :: ColorCorrector -> Lude.Maybe Lude.Natural) (\s a -> s {saturation = a} :: ColorCorrector)
{-# DEPRECATED ccSaturation "Use generic-lens or generic-optics with 'saturation' instead." #-}

-- | Hue in degrees.
--
-- /Note:/ Consider using 'hue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHue :: Lens.Lens' ColorCorrector (Lude.Maybe Lude.Int)
ccHue = Lens.lens (hue :: ColorCorrector -> Lude.Maybe Lude.Int) (\s a -> s {hue = a} :: ColorCorrector)
{-# DEPRECATED ccHue "Use generic-lens or generic-optics with 'hue' instead." #-}

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
--
-- /Note:/ Consider using 'colorSpaceConversion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccColorSpaceConversion :: Lens.Lens' ColorCorrector (Lude.Maybe ColorSpaceConversion)
ccColorSpaceConversion = Lens.lens (colorSpaceConversion :: ColorCorrector -> Lude.Maybe ColorSpaceConversion) (\s a -> s {colorSpaceConversion = a} :: ColorCorrector)
{-# DEPRECATED ccColorSpaceConversion "Use generic-lens or generic-optics with 'colorSpaceConversion' instead." #-}

-- | Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'hdr10Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHdr10Metadata :: Lens.Lens' ColorCorrector (Lude.Maybe Hdr10Metadata)
ccHdr10Metadata = Lens.lens (hdr10Metadata :: ColorCorrector -> Lude.Maybe Hdr10Metadata) (\s a -> s {hdr10Metadata = a} :: ColorCorrector)
{-# DEPRECATED ccHdr10Metadata "Use generic-lens or generic-optics with 'hdr10Metadata' instead." #-}

-- | Contrast level.
--
-- /Note:/ Consider using 'contrast' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContrast :: Lens.Lens' ColorCorrector (Lude.Maybe Lude.Natural)
ccContrast = Lens.lens (contrast :: ColorCorrector -> Lude.Maybe Lude.Natural) (\s a -> s {contrast = a} :: ColorCorrector)
{-# DEPRECATED ccContrast "Use generic-lens or generic-optics with 'contrast' instead." #-}

-- | Brightness level.
--
-- /Note:/ Consider using 'brightness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBrightness :: Lens.Lens' ColorCorrector (Lude.Maybe Lude.Natural)
ccBrightness = Lens.lens (brightness :: ColorCorrector -> Lude.Maybe Lude.Natural) (\s a -> s {brightness = a} :: ColorCorrector)
{-# DEPRECATED ccBrightness "Use generic-lens or generic-optics with 'brightness' instead." #-}

instance Lude.FromJSON ColorCorrector where
  parseJSON =
    Lude.withObject
      "ColorCorrector"
      ( \x ->
          ColorCorrector'
            Lude.<$> (x Lude..:? "saturation")
            Lude.<*> (x Lude..:? "hue")
            Lude.<*> (x Lude..:? "colorSpaceConversion")
            Lude.<*> (x Lude..:? "hdr10Metadata")
            Lude.<*> (x Lude..:? "contrast")
            Lude.<*> (x Lude..:? "brightness")
      )

instance Lude.ToJSON ColorCorrector where
  toJSON ColorCorrector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("saturation" Lude..=) Lude.<$> saturation,
            ("hue" Lude..=) Lude.<$> hue,
            ("colorSpaceConversion" Lude..=) Lude.<$> colorSpaceConversion,
            ("hdr10Metadata" Lude..=) Lude.<$> hdr10Metadata,
            ("contrast" Lude..=) Lude.<$> contrast,
            ("brightness" Lude..=) Lude.<$> brightness
          ]
      )
