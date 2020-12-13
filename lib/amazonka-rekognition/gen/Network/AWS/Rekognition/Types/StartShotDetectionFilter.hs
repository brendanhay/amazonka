{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartShotDetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartShotDetectionFilter
  ( StartShotDetectionFilter (..),

    -- * Smart constructor
    mkStartShotDetectionFilter,

    -- * Lenses
    ssdfMinSegmentConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters for the shot detection segments returned by @GetSegmentDetection@ . For more information, see 'StartSegmentDetectionFilters' .
--
-- /See:/ 'mkStartShotDetectionFilter' smart constructor.
newtype StartShotDetectionFilter = StartShotDetectionFilter'
  { -- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value.
    --
    -- If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
    minSegmentConfidence :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartShotDetectionFilter' with the minimum fields required to make a request.
--
-- * 'minSegmentConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value.
--
-- If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
mkStartShotDetectionFilter ::
  StartShotDetectionFilter
mkStartShotDetectionFilter =
  StartShotDetectionFilter' {minSegmentConfidence = Lude.Nothing}

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value.
--
-- If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minSegmentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfMinSegmentConfidence :: Lens.Lens' StartShotDetectionFilter (Lude.Maybe Lude.Double)
ssdfMinSegmentConfidence = Lens.lens (minSegmentConfidence :: StartShotDetectionFilter -> Lude.Maybe Lude.Double) (\s a -> s {minSegmentConfidence = a} :: StartShotDetectionFilter)
{-# DEPRECATED ssdfMinSegmentConfidence "Use generic-lens or generic-optics with 'minSegmentConfidence' instead." #-}

instance Lude.ToJSON StartShotDetectionFilter where
  toJSON StartShotDetectionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("MinSegmentConfidence" Lude..=) Lude.<$> minSegmentConfidence]
      )
