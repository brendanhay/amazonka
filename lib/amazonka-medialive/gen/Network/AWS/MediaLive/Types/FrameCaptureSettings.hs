{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureSettings
  ( FrameCaptureSettings (..),

    -- * Smart constructor
    mkFrameCaptureSettings,

    -- * Lenses
    fcsCaptureIntervalUnits,
    fcsCaptureInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
import qualified Network.AWS.Prelude as Lude

-- | Frame Capture Settings
--
-- /See:/ 'mkFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { captureIntervalUnits ::
      Lude.Maybe FrameCaptureIntervalUnit,
    captureInterval :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FrameCaptureSettings' with the minimum fields required to make a request.
--
-- * 'captureInterval' - The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
-- * 'captureIntervalUnits' - Unit for the frame capture interval.
mkFrameCaptureSettings ::
  -- | 'captureInterval'
  Lude.Natural ->
  FrameCaptureSettings
mkFrameCaptureSettings pCaptureInterval_ =
  FrameCaptureSettings'
    { captureIntervalUnits = Lude.Nothing,
      captureInterval = pCaptureInterval_
    }

-- | Unit for the frame capture interval.
--
-- /Note:/ Consider using 'captureIntervalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsCaptureIntervalUnits :: Lens.Lens' FrameCaptureSettings (Lude.Maybe FrameCaptureIntervalUnit)
fcsCaptureIntervalUnits = Lens.lens (captureIntervalUnits :: FrameCaptureSettings -> Lude.Maybe FrameCaptureIntervalUnit) (\s a -> s {captureIntervalUnits = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsCaptureIntervalUnits "Use generic-lens or generic-optics with 'captureIntervalUnits' instead." #-}

-- | The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
--
-- /Note:/ Consider using 'captureInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsCaptureInterval :: Lens.Lens' FrameCaptureSettings Lude.Natural
fcsCaptureInterval = Lens.lens (captureInterval :: FrameCaptureSettings -> Lude.Natural) (\s a -> s {captureInterval = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsCaptureInterval "Use generic-lens or generic-optics with 'captureInterval' instead." #-}

instance Lude.FromJSON FrameCaptureSettings where
  parseJSON =
    Lude.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Lude.<$> (x Lude..:? "captureIntervalUnits")
            Lude.<*> (x Lude..: "captureInterval")
      )

instance Lude.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("captureIntervalUnits" Lude..=) Lude.<$> captureIntervalUnits,
            Lude.Just ("captureInterval" Lude..= captureInterval)
          ]
      )
