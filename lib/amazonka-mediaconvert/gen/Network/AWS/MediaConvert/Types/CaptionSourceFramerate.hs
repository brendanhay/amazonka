-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceFramerate
  ( CaptionSourceFramerate (..),

    -- * Smart constructor
    mkCaptionSourceFramerate,

    -- * Lenses
    csfFramerateDenominator,
    csfFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
--
-- /See:/ 'mkCaptionSourceFramerate' smart constructor.
data CaptionSourceFramerate = CaptionSourceFramerate'
  { framerateDenominator ::
      Lude.Maybe Lude.Natural,
    framerateNumerator :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionSourceFramerate' with the minimum fields required to make a request.
--
-- * 'framerateDenominator' - Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
-- * 'framerateNumerator' - Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
mkCaptionSourceFramerate ::
  CaptionSourceFramerate
mkCaptionSourceFramerate =
  CaptionSourceFramerate'
    { framerateDenominator = Lude.Nothing,
      framerateNumerator = Lude.Nothing
    }

-- | Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFramerateDenominator :: Lens.Lens' CaptionSourceFramerate (Lude.Maybe Lude.Natural)
csfFramerateDenominator = Lens.lens (framerateDenominator :: CaptionSourceFramerate -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: CaptionSourceFramerate)
{-# DEPRECATED csfFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFramerateNumerator :: Lens.Lens' CaptionSourceFramerate (Lude.Maybe Lude.Natural)
csfFramerateNumerator = Lens.lens (framerateNumerator :: CaptionSourceFramerate -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: CaptionSourceFramerate)
{-# DEPRECATED csfFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Lude.FromJSON CaptionSourceFramerate where
  parseJSON =
    Lude.withObject
      "CaptionSourceFramerate"
      ( \x ->
          CaptionSourceFramerate'
            Lude.<$> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateNumerator")
      )

instance Lude.ToJSON CaptionSourceFramerate where
  toJSON CaptionSourceFramerate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator
          ]
      )
