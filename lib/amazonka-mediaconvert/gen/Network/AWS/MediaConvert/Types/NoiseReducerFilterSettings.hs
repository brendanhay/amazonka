{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
  ( NoiseReducerFilterSettings (..),

    -- * Smart constructor
    mkNoiseReducerFilterSettings,

    -- * Lenses
    nrfsStrength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for a noise reducer filter
--
-- /See:/ 'mkNoiseReducerFilterSettings' smart constructor.
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { strength ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoiseReducerFilterSettings' with the minimum fields required to make a request.
--
-- * 'strength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
mkNoiseReducerFilterSettings ::
  NoiseReducerFilterSettings
mkNoiseReducerFilterSettings =
  NoiseReducerFilterSettings' {strength = Lude.Nothing}

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrfsStrength :: Lens.Lens' NoiseReducerFilterSettings (Lude.Maybe Lude.Natural)
nrfsStrength = Lens.lens (strength :: NoiseReducerFilterSettings -> Lude.Maybe Lude.Natural) (\s a -> s {strength = a} :: NoiseReducerFilterSettings)
{-# DEPRECATED nrfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

instance Lude.FromJSON NoiseReducerFilterSettings where
  parseJSON =
    Lude.withObject
      "NoiseReducerFilterSettings"
      ( \x ->
          NoiseReducerFilterSettings' Lude.<$> (x Lude..:? "strength")
      )

instance Lude.ToJSON NoiseReducerFilterSettings where
  toJSON NoiseReducerFilterSettings' {..} =
    Lude.object
      (Lude.catMaybes [("strength" Lude..=) Lude.<$> strength])
