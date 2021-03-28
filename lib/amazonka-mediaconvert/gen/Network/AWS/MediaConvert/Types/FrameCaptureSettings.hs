{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.FrameCaptureSettings
  ( FrameCaptureSettings (..)
  -- * Smart constructor
  , mkFrameCaptureSettings
  -- * Lenses
  , fcsFramerateDenominator
  , fcsFramerateNumerator
  , fcsMaxCaptures
  , fcsQuality
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- /See:/ 'mkFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { framerateDenominator :: Core.Maybe Core.Natural
    -- ^ Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
  , maxCaptures :: Core.Maybe Core.Natural
    -- ^ Maximum number of captures (encoded jpg output files).
  , quality :: Core.Maybe Core.Natural
    -- ^ JPEG Quality - a higher value equals higher quality.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FrameCaptureSettings' value with any optional fields omitted.
mkFrameCaptureSettings
    :: FrameCaptureSettings
mkFrameCaptureSettings
  = FrameCaptureSettings'{framerateDenominator = Core.Nothing,
                          framerateNumerator = Core.Nothing, maxCaptures = Core.Nothing,
                          quality = Core.Nothing}

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsFramerateDenominator :: Lens.Lens' FrameCaptureSettings (Core.Maybe Core.Natural)
fcsFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE fcsFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsFramerateNumerator :: Lens.Lens' FrameCaptureSettings (Core.Maybe Core.Natural)
fcsFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE fcsFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Maximum number of captures (encoded jpg output files).
--
-- /Note:/ Consider using 'maxCaptures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsMaxCaptures :: Lens.Lens' FrameCaptureSettings (Core.Maybe Core.Natural)
fcsMaxCaptures = Lens.field @"maxCaptures"
{-# INLINEABLE fcsMaxCaptures #-}
{-# DEPRECATED maxCaptures "Use generic-lens or generic-optics with 'maxCaptures' instead"  #-}

-- | JPEG Quality - a higher value equals higher quality.
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsQuality :: Lens.Lens' FrameCaptureSettings (Core.Maybe Core.Natural)
fcsQuality = Lens.field @"quality"
{-# INLINEABLE fcsQuality #-}
{-# DEPRECATED quality "Use generic-lens or generic-optics with 'quality' instead"  #-}

instance Core.FromJSON FrameCaptureSettings where
        toJSON FrameCaptureSettings{..}
          = Core.object
              (Core.catMaybes
                 [("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("maxCaptures" Core..=) Core.<$> maxCaptures,
                  ("quality" Core..=) Core.<$> quality])

instance Core.FromJSON FrameCaptureSettings where
        parseJSON
          = Core.withObject "FrameCaptureSettings" Core.$
              \ x ->
                FrameCaptureSettings' Core.<$>
                  (x Core..:? "framerateDenominator") Core.<*>
                    x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "maxCaptures"
                    Core.<*> x Core..:? "quality"
