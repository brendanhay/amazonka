{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
  ( NoiseReducerSpatialFilterSettings (..),

    -- * Smart constructor
    mkNoiseReducerSpatialFilterSettings,

    -- * Lenses
    nrsfsStrength,
    nrsfsPostFilterSharpenStrength,
    nrsfsSpeed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'mkNoiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { strength ::
      Lude.Maybe Lude.Natural,
    postFilterSharpenStrength ::
      Lude.Maybe Lude.Natural,
    speed ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoiseReducerSpatialFilterSettings' with the minimum fields required to make a request.
--
-- * 'postFilterSharpenStrength' - Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
-- * 'speed' - The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
-- * 'strength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
mkNoiseReducerSpatialFilterSettings ::
  NoiseReducerSpatialFilterSettings
mkNoiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { strength = Lude.Nothing,
      postFilterSharpenStrength = Lude.Nothing,
      speed = Lude.Nothing
    }

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Lude.Maybe Lude.Natural)
nrsfsStrength = Lens.lens (strength :: NoiseReducerSpatialFilterSettings -> Lude.Maybe Lude.Natural) (\s a -> s {strength = a} :: NoiseReducerSpatialFilterSettings)
{-# DEPRECATED nrsfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

-- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
--
-- /Note:/ Consider using 'postFilterSharpenStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsPostFilterSharpenStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Lude.Maybe Lude.Natural)
nrsfsPostFilterSharpenStrength = Lens.lens (postFilterSharpenStrength :: NoiseReducerSpatialFilterSettings -> Lude.Maybe Lude.Natural) (\s a -> s {postFilterSharpenStrength = a} :: NoiseReducerSpatialFilterSettings)
{-# DEPRECATED nrsfsPostFilterSharpenStrength "Use generic-lens or generic-optics with 'postFilterSharpenStrength' instead." #-}

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
--
-- /Note:/ Consider using 'speed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsSpeed :: Lens.Lens' NoiseReducerSpatialFilterSettings (Lude.Maybe Lude.Int)
nrsfsSpeed = Lens.lens (speed :: NoiseReducerSpatialFilterSettings -> Lude.Maybe Lude.Int) (\s a -> s {speed = a} :: NoiseReducerSpatialFilterSettings)
{-# DEPRECATED nrsfsSpeed "Use generic-lens or generic-optics with 'speed' instead." #-}

instance Lude.FromJSON NoiseReducerSpatialFilterSettings where
  parseJSON =
    Lude.withObject
      "NoiseReducerSpatialFilterSettings"
      ( \x ->
          NoiseReducerSpatialFilterSettings'
            Lude.<$> (x Lude..:? "strength")
            Lude.<*> (x Lude..:? "postFilterSharpenStrength")
            Lude.<*> (x Lude..:? "speed")
      )

instance Lude.ToJSON NoiseReducerSpatialFilterSettings where
  toJSON NoiseReducerSpatialFilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("strength" Lude..=) Lude.<$> strength,
            ("postFilterSharpenStrength" Lude..=)
              Lude.<$> postFilterSharpenStrength,
            ("speed" Lude..=) Lude.<$> speed
          ]
      )
