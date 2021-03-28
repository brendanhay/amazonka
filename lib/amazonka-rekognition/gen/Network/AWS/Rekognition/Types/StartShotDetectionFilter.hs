{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartShotDetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.StartShotDetectionFilter
  ( StartShotDetectionFilter (..)
  -- * Smart constructor
  , mkStartShotDetectionFilter
  -- * Lenses
  , ssdfMinSegmentConfidence
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters for the shot detection segments returned by @GetSegmentDetection@ . For more information, see 'StartSegmentDetectionFilters' .
--
-- /See:/ 'mkStartShotDetectionFilter' smart constructor.
newtype StartShotDetectionFilter = StartShotDetectionFilter'
  { minSegmentConfidence :: Core.Maybe Core.Double
    -- ^ Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value.
--
-- If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartShotDetectionFilter' value with any optional fields omitted.
mkStartShotDetectionFilter
    :: StartShotDetectionFilter
mkStartShotDetectionFilter
  = StartShotDetectionFilter'{minSegmentConfidence = Core.Nothing}

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected segment. Confidence represents how certain Amazon Rekognition is that a segment is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any segments with a confidence level lower than this specified value.
--
-- If you don't specify @MinSegmentConfidence@ , the @GetSegmentDetection@ returns segments with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minSegmentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfMinSegmentConfidence :: Lens.Lens' StartShotDetectionFilter (Core.Maybe Core.Double)
ssdfMinSegmentConfidence = Lens.field @"minSegmentConfidence"
{-# INLINEABLE ssdfMinSegmentConfidence #-}
{-# DEPRECATED minSegmentConfidence "Use generic-lens or generic-optics with 'minSegmentConfidence' instead"  #-}

instance Core.FromJSON StartShotDetectionFilter where
        toJSON StartShotDetectionFilter{..}
          = Core.object
              (Core.catMaybes
                 [("MinSegmentConfidence" Core..=) Core.<$> minSegmentConfidence])
