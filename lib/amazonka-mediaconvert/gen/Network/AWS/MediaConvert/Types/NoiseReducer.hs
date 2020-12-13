{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducer
  ( NoiseReducer (..),

    -- * Smart constructor
    mkNoiseReducer,

    -- * Lenses
    nrTemporalFilterSettings,
    nrSpatialFilterSettings,
    nrFilterSettings,
    nrFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NoiseReducerFilter
import Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import qualified Network.AWS.Prelude as Lude

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
--
-- /See:/ 'mkNoiseReducer' smart constructor.
data NoiseReducer = NoiseReducer'
  { -- | Noise reducer filter settings for temporal filter.
    temporalFilterSettings :: Lude.Maybe NoiseReducerTemporalFilterSettings,
    -- | Noise reducer filter settings for spatial filter.
    spatialFilterSettings :: Lude.Maybe NoiseReducerSpatialFilterSettings,
    -- | Settings for a noise reducer filter
    filterSettings :: Lude.Maybe NoiseReducerFilterSettings,
    -- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
    filter :: Lude.Maybe NoiseReducerFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoiseReducer' with the minimum fields required to make a request.
--
-- * 'temporalFilterSettings' - Noise reducer filter settings for temporal filter.
-- * 'spatialFilterSettings' - Noise reducer filter settings for spatial filter.
-- * 'filterSettings' - Settings for a noise reducer filter
-- * 'filter' - Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
mkNoiseReducer ::
  NoiseReducer
mkNoiseReducer =
  NoiseReducer'
    { temporalFilterSettings = Lude.Nothing,
      spatialFilterSettings = Lude.Nothing,
      filterSettings = Lude.Nothing,
      filter = Lude.Nothing
    }

-- | Noise reducer filter settings for temporal filter.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrTemporalFilterSettings :: Lens.Lens' NoiseReducer (Lude.Maybe NoiseReducerTemporalFilterSettings)
nrTemporalFilterSettings = Lens.lens (temporalFilterSettings :: NoiseReducer -> Lude.Maybe NoiseReducerTemporalFilterSettings) (\s a -> s {temporalFilterSettings = a} :: NoiseReducer)
{-# DEPRECATED nrTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

-- | Noise reducer filter settings for spatial filter.
--
-- /Note:/ Consider using 'spatialFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrSpatialFilterSettings :: Lens.Lens' NoiseReducer (Lude.Maybe NoiseReducerSpatialFilterSettings)
nrSpatialFilterSettings = Lens.lens (spatialFilterSettings :: NoiseReducer -> Lude.Maybe NoiseReducerSpatialFilterSettings) (\s a -> s {spatialFilterSettings = a} :: NoiseReducer)
{-# DEPRECATED nrSpatialFilterSettings "Use generic-lens or generic-optics with 'spatialFilterSettings' instead." #-}

-- | Settings for a noise reducer filter
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrFilterSettings :: Lens.Lens' NoiseReducer (Lude.Maybe NoiseReducerFilterSettings)
nrFilterSettings = Lens.lens (filterSettings :: NoiseReducer -> Lude.Maybe NoiseReducerFilterSettings) (\s a -> s {filterSettings = a} :: NoiseReducer)
{-# DEPRECATED nrFilterSettings "Use generic-lens or generic-optics with 'filterSettings' instead." #-}

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrFilter :: Lens.Lens' NoiseReducer (Lude.Maybe NoiseReducerFilter)
nrFilter = Lens.lens (filter :: NoiseReducer -> Lude.Maybe NoiseReducerFilter) (\s a -> s {filter = a} :: NoiseReducer)
{-# DEPRECATED nrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.FromJSON NoiseReducer where
  parseJSON =
    Lude.withObject
      "NoiseReducer"
      ( \x ->
          NoiseReducer'
            Lude.<$> (x Lude..:? "temporalFilterSettings")
            Lude.<*> (x Lude..:? "spatialFilterSettings")
            Lude.<*> (x Lude..:? "filterSettings")
            Lude.<*> (x Lude..:? "filter")
      )

instance Lude.ToJSON NoiseReducer where
  toJSON NoiseReducer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("temporalFilterSettings" Lude..=)
              Lude.<$> temporalFilterSettings,
            ("spatialFilterSettings" Lude..=) Lude.<$> spatialFilterSettings,
            ("filterSettings" Lude..=) Lude.<$> filterSettings,
            ("filter" Lude..=) Lude.<$> filter
          ]
      )
