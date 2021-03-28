{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Hdr10Metadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Hdr10Metadata
  ( Hdr10Metadata (..)
  -- * Smart constructor
  , mkHdr10Metadata
  -- * Lenses
  , hmBluePrimaryX
  , hmBluePrimaryY
  , hmGreenPrimaryX
  , hmGreenPrimaryY
  , hmMaxContentLightLevel
  , hmMaxFrameAverageLightLevel
  , hmMaxLuminance
  , hmMinLuminance
  , hmRedPrimaryX
  , hmRedPrimaryY
  , hmWhitePointX
  , hmWhitePointY
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use these settings to specify static color calibration metadata, as defined by SMPTE ST 2086. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator.
--
-- /See:/ 'mkHdr10Metadata' smart constructor.
data Hdr10Metadata = Hdr10Metadata'
  { bluePrimaryX :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , bluePrimaryY :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , greenPrimaryX :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , greenPrimaryY :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , maxContentLightLevel :: Core.Maybe Core.Natural
    -- ^ Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.  This setting doesn't have a default value; you must specify a value that is suitable for the content.
  , maxFrameAverageLightLevel :: Core.Maybe Core.Natural
    -- ^ Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter. This setting doesn't have a default value; you must specify a value that is suitable for the content.
  , maxLuminance :: Core.Maybe Core.Natural
    -- ^ Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
  , minLuminance :: Core.Maybe Core.Natural
    -- ^ Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
  , redPrimaryX :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , redPrimaryY :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , whitePointX :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  , whitePointY :: Core.Maybe Core.Natural
    -- ^ HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hdr10Metadata' value with any optional fields omitted.
mkHdr10Metadata
    :: Hdr10Metadata
mkHdr10Metadata
  = Hdr10Metadata'{bluePrimaryX = Core.Nothing,
                   bluePrimaryY = Core.Nothing, greenPrimaryX = Core.Nothing,
                   greenPrimaryY = Core.Nothing, maxContentLightLevel = Core.Nothing,
                   maxFrameAverageLightLevel = Core.Nothing,
                   maxLuminance = Core.Nothing, minLuminance = Core.Nothing,
                   redPrimaryX = Core.Nothing, redPrimaryY = Core.Nothing,
                   whitePointX = Core.Nothing, whitePointY = Core.Nothing}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'bluePrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmBluePrimaryX :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmBluePrimaryX = Lens.field @"bluePrimaryX"
{-# INLINEABLE hmBluePrimaryX #-}
{-# DEPRECATED bluePrimaryX "Use generic-lens or generic-optics with 'bluePrimaryX' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'bluePrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmBluePrimaryY :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmBluePrimaryY = Lens.field @"bluePrimaryY"
{-# INLINEABLE hmBluePrimaryY #-}
{-# DEPRECATED bluePrimaryY "Use generic-lens or generic-optics with 'bluePrimaryY' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'greenPrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmGreenPrimaryX :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmGreenPrimaryX = Lens.field @"greenPrimaryX"
{-# INLINEABLE hmGreenPrimaryX #-}
{-# DEPRECATED greenPrimaryX "Use generic-lens or generic-optics with 'greenPrimaryX' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'greenPrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmGreenPrimaryY :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmGreenPrimaryY = Lens.field @"greenPrimaryY"
{-# INLINEABLE hmGreenPrimaryY #-}
{-# DEPRECATED greenPrimaryY "Use generic-lens or generic-optics with 'greenPrimaryY' instead"  #-}

-- | Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.  This setting doesn't have a default value; you must specify a value that is suitable for the content.
--
-- /Note:/ Consider using 'maxContentLightLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxContentLightLevel :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmMaxContentLightLevel = Lens.field @"maxContentLightLevel"
{-# INLINEABLE hmMaxContentLightLevel #-}
{-# DEPRECATED maxContentLightLevel "Use generic-lens or generic-optics with 'maxContentLightLevel' instead"  #-}

-- | Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter. This setting doesn't have a default value; you must specify a value that is suitable for the content.
--
-- /Note:/ Consider using 'maxFrameAverageLightLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxFrameAverageLightLevel :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmMaxFrameAverageLightLevel = Lens.field @"maxFrameAverageLightLevel"
{-# INLINEABLE hmMaxFrameAverageLightLevel #-}
{-# DEPRECATED maxFrameAverageLightLevel "Use generic-lens or generic-optics with 'maxFrameAverageLightLevel' instead"  #-}

-- | Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
--
-- /Note:/ Consider using 'maxLuminance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxLuminance :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmMaxLuminance = Lens.field @"maxLuminance"
{-# INLINEABLE hmMaxLuminance #-}
{-# DEPRECATED maxLuminance "Use generic-lens or generic-optics with 'maxLuminance' instead"  #-}

-- | Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
--
-- /Note:/ Consider using 'minLuminance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMinLuminance :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmMinLuminance = Lens.field @"minLuminance"
{-# INLINEABLE hmMinLuminance #-}
{-# DEPRECATED minLuminance "Use generic-lens or generic-optics with 'minLuminance' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'redPrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmRedPrimaryX :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmRedPrimaryX = Lens.field @"redPrimaryX"
{-# INLINEABLE hmRedPrimaryX #-}
{-# DEPRECATED redPrimaryX "Use generic-lens or generic-optics with 'redPrimaryX' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'redPrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmRedPrimaryY :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmRedPrimaryY = Lens.field @"redPrimaryY"
{-# INLINEABLE hmRedPrimaryY #-}
{-# DEPRECATED redPrimaryY "Use generic-lens or generic-optics with 'redPrimaryY' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'whitePointX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmWhitePointX :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmWhitePointX = Lens.field @"whitePointX"
{-# INLINEABLE hmWhitePointX #-}
{-# DEPRECATED whitePointX "Use generic-lens or generic-optics with 'whitePointX' instead"  #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'whitePointY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmWhitePointY :: Lens.Lens' Hdr10Metadata (Core.Maybe Core.Natural)
hmWhitePointY = Lens.field @"whitePointY"
{-# INLINEABLE hmWhitePointY #-}
{-# DEPRECATED whitePointY "Use generic-lens or generic-optics with 'whitePointY' instead"  #-}

instance Core.FromJSON Hdr10Metadata where
        toJSON Hdr10Metadata{..}
          = Core.object
              (Core.catMaybes
                 [("bluePrimaryX" Core..=) Core.<$> bluePrimaryX,
                  ("bluePrimaryY" Core..=) Core.<$> bluePrimaryY,
                  ("greenPrimaryX" Core..=) Core.<$> greenPrimaryX,
                  ("greenPrimaryY" Core..=) Core.<$> greenPrimaryY,
                  ("maxContentLightLevel" Core..=) Core.<$> maxContentLightLevel,
                  ("maxFrameAverageLightLevel" Core..=) Core.<$>
                    maxFrameAverageLightLevel,
                  ("maxLuminance" Core..=) Core.<$> maxLuminance,
                  ("minLuminance" Core..=) Core.<$> minLuminance,
                  ("redPrimaryX" Core..=) Core.<$> redPrimaryX,
                  ("redPrimaryY" Core..=) Core.<$> redPrimaryY,
                  ("whitePointX" Core..=) Core.<$> whitePointX,
                  ("whitePointY" Core..=) Core.<$> whitePointY])

instance Core.FromJSON Hdr10Metadata where
        parseJSON
          = Core.withObject "Hdr10Metadata" Core.$
              \ x ->
                Hdr10Metadata' Core.<$>
                  (x Core..:? "bluePrimaryX") Core.<*> x Core..:? "bluePrimaryY"
                    Core.<*> x Core..:? "greenPrimaryX"
                    Core.<*> x Core..:? "greenPrimaryY"
                    Core.<*> x Core..:? "maxContentLightLevel"
                    Core.<*> x Core..:? "maxFrameAverageLightLevel"
                    Core.<*> x Core..:? "maxLuminance"
                    Core.<*> x Core..:? "minLuminance"
                    Core.<*> x Core..:? "redPrimaryX"
                    Core.<*> x Core..:? "redPrimaryY"
                    Core.<*> x Core..:? "whitePointX"
                    Core.<*> x Core..:? "whitePointY"
