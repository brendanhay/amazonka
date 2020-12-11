-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TechnicalCueSegment
  ( TechnicalCueSegment (..),

    -- * Smart constructor
    mkTechnicalCueSegment,

    -- * Lenses
    tcsConfidence,
    tcsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.TechnicalCueType

-- | Information about a technical cue segment. For more information, see 'SegmentDetection' .
--
-- /See:/ 'mkTechnicalCueSegment' smart constructor.
data TechnicalCueSegment = TechnicalCueSegment'
  { confidence ::
      Lude.Maybe Lude.Double,
    type' :: Lude.Maybe TechnicalCueType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TechnicalCueSegment' with the minimum fields required to make a request.
--
-- * 'confidence' - The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
-- * 'type'' - The type of the technical cue.
mkTechnicalCueSegment ::
  TechnicalCueSegment
mkTechnicalCueSegment =
  TechnicalCueSegment'
    { confidence = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The confidence that Amazon Rekognition Video has in the accuracy of the detected segment.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsConfidence :: Lens.Lens' TechnicalCueSegment (Lude.Maybe Lude.Double)
tcsConfidence = Lens.lens (confidence :: TechnicalCueSegment -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: TechnicalCueSegment)
{-# DEPRECATED tcsConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The type of the technical cue.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsType :: Lens.Lens' TechnicalCueSegment (Lude.Maybe TechnicalCueType)
tcsType = Lens.lens (type' :: TechnicalCueSegment -> Lude.Maybe TechnicalCueType) (\s a -> s {type' = a} :: TechnicalCueSegment)
{-# DEPRECATED tcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON TechnicalCueSegment where
  parseJSON =
    Lude.withObject
      "TechnicalCueSegment"
      ( \x ->
          TechnicalCueSegment'
            Lude.<$> (x Lude..:? "Confidence") Lude.<*> (x Lude..:? "Type")
      )
