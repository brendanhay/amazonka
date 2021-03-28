{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoPreprocessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.VideoPreprocessor
  ( VideoPreprocessor (..)
  -- * Smart constructor
  , mkVideoPreprocessor
  -- * Lenses
  , vpColorCorrector
  , vpDeinterlacer
  , vpDolbyVision
  , vpImageInserter
  , vpNoiseReducer
  , vpPartnerWatermarking
  , vpTimecodeBurnin
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.ColorCorrector as Types
import qualified Network.AWS.MediaConvert.Types.Deinterlacer as Types
import qualified Network.AWS.MediaConvert.Types.DolbyVision as Types
import qualified Network.AWS.MediaConvert.Types.ImageInserter as Types
import qualified Network.AWS.MediaConvert.Types.NoiseReducer as Types
import qualified Network.AWS.MediaConvert.Types.PartnerWatermarking as Types
import qualified Network.AWS.MediaConvert.Types.TimecodeBurnin as Types
import qualified Network.AWS.Prelude as Core

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /See:/ 'mkVideoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { colorCorrector :: Core.Maybe Types.ColorCorrector
    -- ^ Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
  , deinterlacer :: Core.Maybe Types.Deinterlacer
    -- ^ Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
  , dolbyVision :: Core.Maybe Types.DolbyVision
    -- ^ Enable Dolby Vision feature to produce Dolby Vision compatible video output.
  , imageInserter :: Core.Maybe Types.ImageInserter
    -- ^ Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
  , noiseReducer :: Core.Maybe Types.NoiseReducer
    -- ^ Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
  , partnerWatermarking :: Core.Maybe Types.PartnerWatermarking
    -- ^ If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
  , timecodeBurnin :: Core.Maybe Types.TimecodeBurnin
    -- ^ Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoPreprocessor' value with any optional fields omitted.
mkVideoPreprocessor
    :: VideoPreprocessor
mkVideoPreprocessor
  = VideoPreprocessor'{colorCorrector = Core.Nothing,
                       deinterlacer = Core.Nothing, dolbyVision = Core.Nothing,
                       imageInserter = Core.Nothing, noiseReducer = Core.Nothing,
                       partnerWatermarking = Core.Nothing, timecodeBurnin = Core.Nothing}

-- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'colorCorrector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpColorCorrector :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.ColorCorrector)
vpColorCorrector = Lens.field @"colorCorrector"
{-# INLINEABLE vpColorCorrector #-}
{-# DEPRECATED colorCorrector "Use generic-lens or generic-optics with 'colorCorrector' instead"  #-}

-- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
--
-- /Note:/ Consider using 'deinterlacer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpDeinterlacer :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.Deinterlacer)
vpDeinterlacer = Lens.field @"deinterlacer"
{-# INLINEABLE vpDeinterlacer #-}
{-# DEPRECATED deinterlacer "Use generic-lens or generic-optics with 'deinterlacer' instead"  #-}

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video output.
--
-- /Note:/ Consider using 'dolbyVision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpDolbyVision :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.DolbyVision)
vpDolbyVision = Lens.field @"dolbyVision"
{-# INLINEABLE vpDolbyVision #-}
{-# DEPRECATED dolbyVision "Use generic-lens or generic-optics with 'dolbyVision' instead"  #-}

-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpImageInserter :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.ImageInserter)
vpImageInserter = Lens.field @"imageInserter"
{-# INLINEABLE vpImageInserter #-}
{-# DEPRECATED imageInserter "Use generic-lens or generic-optics with 'imageInserter' instead"  #-}

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'noiseReducer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpNoiseReducer :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.NoiseReducer)
vpNoiseReducer = Lens.field @"noiseReducer"
{-# INLINEABLE vpNoiseReducer #-}
{-# DEPRECATED noiseReducer "Use generic-lens or generic-optics with 'noiseReducer' instead"  #-}

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
--
-- /Note:/ Consider using 'partnerWatermarking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpPartnerWatermarking :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.PartnerWatermarking)
vpPartnerWatermarking = Lens.field @"partnerWatermarking"
{-# INLINEABLE vpPartnerWatermarking #-}
{-# DEPRECATED partnerWatermarking "Use generic-lens or generic-optics with 'partnerWatermarking' instead"  #-}

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /Note:/ Consider using 'timecodeBurnin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpTimecodeBurnin :: Lens.Lens' VideoPreprocessor (Core.Maybe Types.TimecodeBurnin)
vpTimecodeBurnin = Lens.field @"timecodeBurnin"
{-# INLINEABLE vpTimecodeBurnin #-}
{-# DEPRECATED timecodeBurnin "Use generic-lens or generic-optics with 'timecodeBurnin' instead"  #-}

instance Core.FromJSON VideoPreprocessor where
        toJSON VideoPreprocessor{..}
          = Core.object
              (Core.catMaybes
                 [("colorCorrector" Core..=) Core.<$> colorCorrector,
                  ("deinterlacer" Core..=) Core.<$> deinterlacer,
                  ("dolbyVision" Core..=) Core.<$> dolbyVision,
                  ("imageInserter" Core..=) Core.<$> imageInserter,
                  ("noiseReducer" Core..=) Core.<$> noiseReducer,
                  ("partnerWatermarking" Core..=) Core.<$> partnerWatermarking,
                  ("timecodeBurnin" Core..=) Core.<$> timecodeBurnin])

instance Core.FromJSON VideoPreprocessor where
        parseJSON
          = Core.withObject "VideoPreprocessor" Core.$
              \ x ->
                VideoPreprocessor' Core.<$>
                  (x Core..:? "colorCorrector") Core.<*> x Core..:? "deinterlacer"
                    Core.<*> x Core..:? "dolbyVision"
                    Core.<*> x Core..:? "imageInserter"
                    Core.<*> x Core..:? "noiseReducer"
                    Core.<*> x Core..:? "partnerWatermarking"
                    Core.<*> x Core..:? "timecodeBurnin"
