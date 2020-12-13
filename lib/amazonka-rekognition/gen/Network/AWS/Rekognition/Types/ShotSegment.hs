{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ShotSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ShotSegment
  ( ShotSegment (..),

    -- * Smart constructor
    mkShotSegment,

    -- * Lenses
    ssConfidence,
    ssIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a shot detection segment detected in a video. For more information, see 'SegmentDetection' .
--
-- /See:/ 'mkShotSegment' smart constructor.
data ShotSegment = ShotSegment'
  { -- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
    confidence :: Lude.Maybe Lude.Double,
    -- | An Identifier for a shot detection segment detected in a video.
    index :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShotSegment' with the minimum fields required to make a request.
--
-- * 'confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
-- * 'index' - An Identifier for a shot detection segment detected in a video.
mkShotSegment ::
  ShotSegment
mkShotSegment =
  ShotSegment' {confidence = Lude.Nothing, index = Lude.Nothing}

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssConfidence :: Lens.Lens' ShotSegment (Lude.Maybe Lude.Double)
ssConfidence = Lens.lens (confidence :: ShotSegment -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ShotSegment)
{-# DEPRECATED ssConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | An Identifier for a shot detection segment detected in a video.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIndex :: Lens.Lens' ShotSegment (Lude.Maybe Lude.Natural)
ssIndex = Lens.lens (index :: ShotSegment -> Lude.Maybe Lude.Natural) (\s a -> s {index = a} :: ShotSegment)
{-# DEPRECATED ssIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON ShotSegment where
  parseJSON =
    Lude.withObject
      "ShotSegment"
      ( \x ->
          ShotSegment'
            Lude.<$> (x Lude..:? "Confidence") Lude.<*> (x Lude..:? "Index")
      )
