{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorCorrector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorCorrector
  ( ColorCorrector (..)
  -- * Smart constructor
  , mkColorCorrector
  -- * Lenses
  , ccBrightness
  , ccColorSpaceConversion
  , ccContrast
  , ccHdr10Metadata
  , ccHue
  , ccSaturation
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.ColorSpaceConversion as Types
import qualified Network.AWS.MediaConvert.Types.Hdr10Metadata as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for color correction.
--
-- /See:/ 'mkColorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { brightness :: Core.Maybe Core.Natural
    -- ^ Brightness level.
  , colorSpaceConversion :: Core.Maybe Types.ColorSpaceConversion
    -- ^ Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
  , contrast :: Core.Maybe Core.Natural
    -- ^ Contrast level.
  , hdr10Metadata :: Core.Maybe Types.Hdr10Metadata
    -- ^ Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
  , hue :: Core.Maybe Core.Int
    -- ^ Hue in degrees.
  , saturation :: Core.Maybe Core.Natural
    -- ^ Saturation level.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ColorCorrector' value with any optional fields omitted.
mkColorCorrector
    :: ColorCorrector
mkColorCorrector
  = ColorCorrector'{brightness = Core.Nothing,
                    colorSpaceConversion = Core.Nothing, contrast = Core.Nothing,
                    hdr10Metadata = Core.Nothing, hue = Core.Nothing,
                    saturation = Core.Nothing}

-- | Brightness level.
--
-- /Note:/ Consider using 'brightness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBrightness :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
ccBrightness = Lens.field @"brightness"
{-# INLINEABLE ccBrightness #-}
{-# DEPRECATED brightness "Use generic-lens or generic-optics with 'brightness' instead"  #-}

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
--
-- /Note:/ Consider using 'colorSpaceConversion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccColorSpaceConversion :: Lens.Lens' ColorCorrector (Core.Maybe Types.ColorSpaceConversion)
ccColorSpaceConversion = Lens.field @"colorSpaceConversion"
{-# INLINEABLE ccColorSpaceConversion #-}
{-# DEPRECATED colorSpaceConversion "Use generic-lens or generic-optics with 'colorSpaceConversion' instead"  #-}

-- | Contrast level.
--
-- /Note:/ Consider using 'contrast' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContrast :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
ccContrast = Lens.field @"contrast"
{-# INLINEABLE ccContrast #-}
{-# DEPRECATED contrast "Use generic-lens or generic-optics with 'contrast' instead"  #-}

-- | Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- /Note:/ Consider using 'hdr10Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHdr10Metadata :: Lens.Lens' ColorCorrector (Core.Maybe Types.Hdr10Metadata)
ccHdr10Metadata = Lens.field @"hdr10Metadata"
{-# INLINEABLE ccHdr10Metadata #-}
{-# DEPRECATED hdr10Metadata "Use generic-lens or generic-optics with 'hdr10Metadata' instead"  #-}

-- | Hue in degrees.
--
-- /Note:/ Consider using 'hue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHue :: Lens.Lens' ColorCorrector (Core.Maybe Core.Int)
ccHue = Lens.field @"hue"
{-# INLINEABLE ccHue #-}
{-# DEPRECATED hue "Use generic-lens or generic-optics with 'hue' instead"  #-}

-- | Saturation level.
--
-- /Note:/ Consider using 'saturation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSaturation :: Lens.Lens' ColorCorrector (Core.Maybe Core.Natural)
ccSaturation = Lens.field @"saturation"
{-# INLINEABLE ccSaturation #-}
{-# DEPRECATED saturation "Use generic-lens or generic-optics with 'saturation' instead"  #-}

instance Core.FromJSON ColorCorrector where
        toJSON ColorCorrector{..}
          = Core.object
              (Core.catMaybes
                 [("brightness" Core..=) Core.<$> brightness,
                  ("colorSpaceConversion" Core..=) Core.<$> colorSpaceConversion,
                  ("contrast" Core..=) Core.<$> contrast,
                  ("hdr10Metadata" Core..=) Core.<$> hdr10Metadata,
                  ("hue" Core..=) Core.<$> hue,
                  ("saturation" Core..=) Core.<$> saturation])

instance Core.FromJSON ColorCorrector where
        parseJSON
          = Core.withObject "ColorCorrector" Core.$
              \ x ->
                ColorCorrector' Core.<$>
                  (x Core..:? "brightness") Core.<*>
                    x Core..:? "colorSpaceConversion"
                    Core.<*> x Core..:? "contrast"
                    Core.<*> x Core..:? "hdr10Metadata"
                    Core.<*> x Core..:? "hue"
                    Core.<*> x Core..:? "saturation"
