-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.LabelDetection
  ( LabelDetection (..),

    -- * Smart constructor
    mkLabelDetection,

    -- * Lenses
    ldLabel,
    ldTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Label

-- | Information about a label detected in a video analysis request and the time the label was detected in the video.
--
-- /See:/ 'mkLabelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { label :: Lude.Maybe Label,
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

-- | Creates a value of 'LabelDetection' with the minimum fields required to make a request.
--
-- * 'label' - Details about the detected label.
-- * 'timestamp' - Time, in milliseconds from the start of the video, that the label was detected.
mkLabelDetection ::
  LabelDetection
mkLabelDetection =
  LabelDetection' {label = Lude.Nothing, timestamp = Lude.Nothing}

-- | Details about the detected label.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLabel :: Lens.Lens' LabelDetection (Lude.Maybe Label)
ldLabel = Lens.lens (label :: LabelDetection -> Lude.Maybe Label) (\s a -> s {label = a} :: LabelDetection)
{-# DEPRECATED ldLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | Time, in milliseconds from the start of the video, that the label was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldTimestamp :: Lens.Lens' LabelDetection (Lude.Maybe Lude.Integer)
ldTimestamp = Lens.lens (timestamp :: LabelDetection -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: LabelDetection)
{-# DEPRECATED ldTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON LabelDetection where
  parseJSON =
    Lude.withObject
      "LabelDetection"
      ( \x ->
          LabelDetection'
            Lude.<$> (x Lude..:? "Label") Lude.<*> (x Lude..:? "Timestamp")
      )
