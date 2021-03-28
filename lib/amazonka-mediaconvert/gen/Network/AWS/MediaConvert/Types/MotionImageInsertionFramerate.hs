{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
  ( MotionImageInsertionFramerate (..)
  -- * Smart constructor
  , mkMotionImageInsertionFramerate
  -- * Lenses
  , miifFramerateDenominator
  , miifFramerateNumerator
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For motion overlays that don't have a built-in frame rate, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. The overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /See:/ 'mkMotionImageInsertionFramerate' smart constructor.
data MotionImageInsertionFramerate = MotionImageInsertionFramerate'
  { framerateDenominator :: Core.Maybe Core.Natural
    -- ^ The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MotionImageInsertionFramerate' value with any optional fields omitted.
mkMotionImageInsertionFramerate
    :: MotionImageInsertionFramerate
mkMotionImageInsertionFramerate
  = MotionImageInsertionFramerate'{framerateDenominator =
                                     Core.Nothing,
                                   framerateNumerator = Core.Nothing}

-- | The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifFramerateDenominator :: Lens.Lens' MotionImageInsertionFramerate (Core.Maybe Core.Natural)
miifFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE miifFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifFramerateNumerator :: Lens.Lens' MotionImageInsertionFramerate (Core.Maybe Core.Natural)
miifFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE miifFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

instance Core.FromJSON MotionImageInsertionFramerate where
        toJSON MotionImageInsertionFramerate{..}
          = Core.object
              (Core.catMaybes
                 [("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator])

instance Core.FromJSON MotionImageInsertionFramerate where
        parseJSON
          = Core.withObject "MotionImageInsertionFramerate" Core.$
              \ x ->
                MotionImageInsertionFramerate' Core.<$>
                  (x Core..:? "framerateDenominator") Core.<*>
                    x Core..:? "framerateNumerator"
