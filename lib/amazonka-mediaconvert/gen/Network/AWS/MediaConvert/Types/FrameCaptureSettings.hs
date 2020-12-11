-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FrameCaptureSettings
  ( FrameCaptureSettings (..),

    -- * Smart constructor
    mkFrameCaptureSettings,

    -- * Lenses
    fcsQuality,
    fcsFramerateDenominator,
    fcsMaxCaptures,
    fcsFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- /See:/ 'mkFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { quality ::
      Lude.Maybe Lude.Natural,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    maxCaptures :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'FrameCaptureSettings' with the minimum fields required to make a request.
--
-- * 'framerateDenominator' - Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
-- * 'framerateNumerator' - Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
-- * 'maxCaptures' - Maximum number of captures (encoded jpg output files).
-- * 'quality' - JPEG Quality - a higher value equals higher quality.
mkFrameCaptureSettings ::
  FrameCaptureSettings
mkFrameCaptureSettings =
  FrameCaptureSettings'
    { quality = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      maxCaptures = Lude.Nothing,
      framerateNumerator = Lude.Nothing
    }

-- | JPEG Quality - a higher value equals higher quality.
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsQuality :: Lens.Lens' FrameCaptureSettings (Lude.Maybe Lude.Natural)
fcsQuality = Lens.lens (quality :: FrameCaptureSettings -> Lude.Maybe Lude.Natural) (\s a -> s {quality = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsQuality "Use generic-lens or generic-optics with 'quality' instead." #-}

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsFramerateDenominator :: Lens.Lens' FrameCaptureSettings (Lude.Maybe Lude.Natural)
fcsFramerateDenominator = Lens.lens (framerateDenominator :: FrameCaptureSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Maximum number of captures (encoded jpg output files).
--
-- /Note:/ Consider using 'maxCaptures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsMaxCaptures :: Lens.Lens' FrameCaptureSettings (Lude.Maybe Lude.Natural)
fcsMaxCaptures = Lens.lens (maxCaptures :: FrameCaptureSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maxCaptures = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsMaxCaptures "Use generic-lens or generic-optics with 'maxCaptures' instead." #-}

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsFramerateNumerator :: Lens.Lens' FrameCaptureSettings (Lude.Maybe Lude.Natural)
fcsFramerateNumerator = Lens.lens (framerateNumerator :: FrameCaptureSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: FrameCaptureSettings)
{-# DEPRECATED fcsFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Lude.FromJSON FrameCaptureSettings where
  parseJSON =
    Lude.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Lude.<$> (x Lude..:? "quality")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "maxCaptures")
            Lude.<*> (x Lude..:? "framerateNumerator")
      )

instance Lude.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("quality" Lude..=) Lude.<$> quality,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("maxCaptures" Lude..=) Lude.<$> maxCaptures,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator
          ]
      )
