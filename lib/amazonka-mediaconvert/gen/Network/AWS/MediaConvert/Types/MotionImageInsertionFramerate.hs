-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
  ( MotionImageInsertionFramerate (..),

    -- * Smart constructor
    mkMotionImageInsertionFramerate,

    -- * Lenses
    miifFramerateDenominator,
    miifFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For motion overlays that don't have a built-in frame rate, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. The overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /See:/ 'mkMotionImageInsertionFramerate' smart constructor.
data MotionImageInsertionFramerate = MotionImageInsertionFramerate'
  { framerateDenominator ::
      Lude.Maybe Lude.Natural,
    framerateNumerator ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MotionImageInsertionFramerate' with the minimum fields required to make a request.
--
-- * 'framerateDenominator' - The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
-- * 'framerateNumerator' - The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
mkMotionImageInsertionFramerate ::
  MotionImageInsertionFramerate
mkMotionImageInsertionFramerate =
  MotionImageInsertionFramerate'
    { framerateDenominator =
        Lude.Nothing,
      framerateNumerator = Lude.Nothing
    }

-- | The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifFramerateDenominator :: Lens.Lens' MotionImageInsertionFramerate (Lude.Maybe Lude.Natural)
miifFramerateDenominator = Lens.lens (framerateDenominator :: MotionImageInsertionFramerate -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: MotionImageInsertionFramerate)
{-# DEPRECATED miifFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miifFramerateNumerator :: Lens.Lens' MotionImageInsertionFramerate (Lude.Maybe Lude.Natural)
miifFramerateNumerator = Lens.lens (framerateNumerator :: MotionImageInsertionFramerate -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: MotionImageInsertionFramerate)
{-# DEPRECATED miifFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Lude.FromJSON MotionImageInsertionFramerate where
  parseJSON =
    Lude.withObject
      "MotionImageInsertionFramerate"
      ( \x ->
          MotionImageInsertionFramerate'
            Lude.<$> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateNumerator")
      )

instance Lude.ToJSON MotionImageInsertionFramerate where
  toJSON MotionImageInsertionFramerate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator
          ]
      )
