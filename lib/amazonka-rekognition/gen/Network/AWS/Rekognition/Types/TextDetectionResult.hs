{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TextDetectionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextDetectionResult
  ( TextDetectionResult (..),

    -- * Smart constructor
    mkTextDetectionResult,

    -- * Lenses
    tdrTextDetection,
    tdrTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.TextDetection

-- | Information about text detected in a video. Incudes the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- /See:/ 'mkTextDetectionResult' smart constructor.
data TextDetectionResult = TextDetectionResult'
  { textDetection ::
      Lude.Maybe TextDetection,
    timestamp :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextDetectionResult' with the minimum fields required to make a request.
--
-- * 'textDetection' - Details about text detected in a video.
-- * 'timestamp' - The time, in milliseconds from the start of the video, that the text was detected.
mkTextDetectionResult ::
  TextDetectionResult
mkTextDetectionResult =
  TextDetectionResult'
    { textDetection = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | Details about text detected in a video.
--
-- /Note:/ Consider using 'textDetection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrTextDetection :: Lens.Lens' TextDetectionResult (Lude.Maybe TextDetection)
tdrTextDetection = Lens.lens (textDetection :: TextDetectionResult -> Lude.Maybe TextDetection) (\s a -> s {textDetection = a} :: TextDetectionResult)
{-# DEPRECATED tdrTextDetection "Use generic-lens or generic-optics with 'textDetection' instead." #-}

-- | The time, in milliseconds from the start of the video, that the text was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrTimestamp :: Lens.Lens' TextDetectionResult (Lude.Maybe Lude.Integer)
tdrTimestamp = Lens.lens (timestamp :: TextDetectionResult -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: TextDetectionResult)
{-# DEPRECATED tdrTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON TextDetectionResult where
  parseJSON =
    Lude.withObject
      "TextDetectionResult"
      ( \x ->
          TextDetectionResult'
            Lude.<$> (x Lude..:? "TextDetection") Lude.<*> (x Lude..:? "Timestamp")
      )
