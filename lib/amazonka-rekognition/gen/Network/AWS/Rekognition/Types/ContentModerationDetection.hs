-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentModerationDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentModerationDetection
  ( ContentModerationDetection (..),

    -- * Smart constructor
    mkContentModerationDetection,

    -- * Lenses
    cmdModerationLabel,
    cmdTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.ModerationLabel

-- | Information about an unsafe content label detection in a stored video.
--
-- /See:/ 'mkContentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { moderationLabel ::
      Lude.Maybe ModerationLabel,
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

-- | Creates a value of 'ContentModerationDetection' with the minimum fields required to make a request.
--
-- * 'moderationLabel' - The unsafe content label detected by in the stored video.
-- * 'timestamp' - Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
mkContentModerationDetection ::
  ContentModerationDetection
mkContentModerationDetection =
  ContentModerationDetection'
    { moderationLabel = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The unsafe content label detected by in the stored video.
--
-- /Note:/ Consider using 'moderationLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdModerationLabel :: Lens.Lens' ContentModerationDetection (Lude.Maybe ModerationLabel)
cmdModerationLabel = Lens.lens (moderationLabel :: ContentModerationDetection -> Lude.Maybe ModerationLabel) (\s a -> s {moderationLabel = a} :: ContentModerationDetection)
{-# DEPRECATED cmdModerationLabel "Use generic-lens or generic-optics with 'moderationLabel' instead." #-}

-- | Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdTimestamp :: Lens.Lens' ContentModerationDetection (Lude.Maybe Lude.Integer)
cmdTimestamp = Lens.lens (timestamp :: ContentModerationDetection -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: ContentModerationDetection)
{-# DEPRECATED cmdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON ContentModerationDetection where
  parseJSON =
    Lude.withObject
      "ContentModerationDetection"
      ( \x ->
          ContentModerationDetection'
            Lude.<$> (x Lude..:? "ModerationLabel") Lude.<*> (x Lude..:? "Timestamp")
      )
