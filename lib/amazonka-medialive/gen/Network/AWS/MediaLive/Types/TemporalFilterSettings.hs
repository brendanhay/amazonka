-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterSettings
  ( TemporalFilterSettings (..),

    -- * Smart constructor
    mkTemporalFilterSettings,

    -- * Lenses
    tfsStrength,
    tfsPostFilterSharpening,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
import Network.AWS.MediaLive.Types.TemporalFilterStrength
import qualified Network.AWS.Prelude as Lude

-- | Temporal Filter Settings
--
-- /See:/ 'mkTemporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'
  { strength ::
      Lude.Maybe TemporalFilterStrength,
    postFilterSharpening ::
      Lude.Maybe TemporalFilterPostFilterSharpening
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemporalFilterSettings' with the minimum fields required to make a request.
--
-- * 'postFilterSharpening' - If you enable this filter, the results are the following:
--
-- - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source.
-- - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
-- * 'strength' - Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
mkTemporalFilterSettings ::
  TemporalFilterSettings
mkTemporalFilterSettings =
  TemporalFilterSettings'
    { strength = Lude.Nothing,
      postFilterSharpening = Lude.Nothing
    }

-- | Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfsStrength :: Lens.Lens' TemporalFilterSettings (Lude.Maybe TemporalFilterStrength)
tfsStrength = Lens.lens (strength :: TemporalFilterSettings -> Lude.Maybe TemporalFilterStrength) (\s a -> s {strength = a} :: TemporalFilterSettings)
{-# DEPRECATED tfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

-- | If you enable this filter, the results are the following:
--
-- - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source.
-- - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
--
-- /Note:/ Consider using 'postFilterSharpening' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfsPostFilterSharpening :: Lens.Lens' TemporalFilterSettings (Lude.Maybe TemporalFilterPostFilterSharpening)
tfsPostFilterSharpening = Lens.lens (postFilterSharpening :: TemporalFilterSettings -> Lude.Maybe TemporalFilterPostFilterSharpening) (\s a -> s {postFilterSharpening = a} :: TemporalFilterSettings)
{-# DEPRECATED tfsPostFilterSharpening "Use generic-lens or generic-optics with 'postFilterSharpening' instead." #-}

instance Lude.FromJSON TemporalFilterSettings where
  parseJSON =
    Lude.withObject
      "TemporalFilterSettings"
      ( \x ->
          TemporalFilterSettings'
            Lude.<$> (x Lude..:? "strength")
            Lude.<*> (x Lude..:? "postFilterSharpening")
      )

instance Lude.ToJSON TemporalFilterSettings where
  toJSON TemporalFilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("strength" Lude..=) Lude.<$> strength,
            ("postFilterSharpening" Lude..=) Lude.<$> postFilterSharpening
          ]
      )
