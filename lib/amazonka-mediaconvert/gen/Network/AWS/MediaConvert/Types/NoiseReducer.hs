{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NoiseReducer
  ( NoiseReducer (..)
  -- * Smart constructor
  , mkNoiseReducer
  -- * Lenses
  , nrFilter
  , nrFilterSettings
  , nrSpatialFilterSettings
  , nrTemporalFilterSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.NoiseReducerFilter as Types
import qualified Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings as Types
import qualified Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings as Types
import qualified Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
--
-- /See:/ 'mkNoiseReducer' smart constructor.
data NoiseReducer = NoiseReducer'
  { filter :: Core.Maybe Types.NoiseReducerFilter
    -- ^ Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
  , filterSettings :: Core.Maybe Types.NoiseReducerFilterSettings
    -- ^ Settings for a noise reducer filter
  , spatialFilterSettings :: Core.Maybe Types.NoiseReducerSpatialFilterSettings
    -- ^ Noise reducer filter settings for spatial filter.
  , temporalFilterSettings :: Core.Maybe Types.NoiseReducerTemporalFilterSettings
    -- ^ Noise reducer filter settings for temporal filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NoiseReducer' value with any optional fields omitted.
mkNoiseReducer
    :: NoiseReducer
mkNoiseReducer
  = NoiseReducer'{filter = Core.Nothing,
                  filterSettings = Core.Nothing,
                  spatialFilterSettings = Core.Nothing,
                  temporalFilterSettings = Core.Nothing}

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrFilter :: Lens.Lens' NoiseReducer (Core.Maybe Types.NoiseReducerFilter)
nrFilter = Lens.field @"filter"
{-# INLINEABLE nrFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Settings for a noise reducer filter
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrFilterSettings :: Lens.Lens' NoiseReducer (Core.Maybe Types.NoiseReducerFilterSettings)
nrFilterSettings = Lens.field @"filterSettings"
{-# INLINEABLE nrFilterSettings #-}
{-# DEPRECATED filterSettings "Use generic-lens or generic-optics with 'filterSettings' instead"  #-}

-- | Noise reducer filter settings for spatial filter.
--
-- /Note:/ Consider using 'spatialFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrSpatialFilterSettings :: Lens.Lens' NoiseReducer (Core.Maybe Types.NoiseReducerSpatialFilterSettings)
nrSpatialFilterSettings = Lens.field @"spatialFilterSettings"
{-# INLINEABLE nrSpatialFilterSettings #-}
{-# DEPRECATED spatialFilterSettings "Use generic-lens or generic-optics with 'spatialFilterSettings' instead"  #-}

-- | Noise reducer filter settings for temporal filter.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrTemporalFilterSettings :: Lens.Lens' NoiseReducer (Core.Maybe Types.NoiseReducerTemporalFilterSettings)
nrTemporalFilterSettings = Lens.field @"temporalFilterSettings"
{-# INLINEABLE nrTemporalFilterSettings #-}
{-# DEPRECATED temporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead"  #-}

instance Core.FromJSON NoiseReducer where
        toJSON NoiseReducer{..}
          = Core.object
              (Core.catMaybes
                 [("filter" Core..=) Core.<$> filter,
                  ("filterSettings" Core..=) Core.<$> filterSettings,
                  ("spatialFilterSettings" Core..=) Core.<$> spatialFilterSettings,
                  ("temporalFilterSettings" Core..=) Core.<$>
                    temporalFilterSettings])

instance Core.FromJSON NoiseReducer where
        parseJSON
          = Core.withObject "NoiseReducer" Core.$
              \ x ->
                NoiseReducer' Core.<$>
                  (x Core..:? "filter") Core.<*> x Core..:? "filterSettings" Core.<*>
                    x Core..:? "spatialFilterSettings"
                    Core.<*> x Core..:? "temporalFilterSettings"
