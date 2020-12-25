{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.DetectionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectionFilter
  ( DetectionFilter (..),

    -- * Smart constructor
    mkDetectionFilter,

    -- * Lenses
    dfMinBoundingBoxHeight,
    dfMinBoundingBoxWidth,
    dfMinConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of parameters that allow you to filter out certain results from your returned results.
--
-- /See:/ 'mkDetectionFilter' smart constructor.
data DetectionFilter = DetectionFilter'
  { -- | Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
    minBoundingBoxHeight :: Core.Maybe Core.Double,
    -- | Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
    minBoundingBoxWidth :: Core.Maybe Core.Double,
    -- | Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
    minConfidence :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectionFilter' value with any optional fields omitted.
mkDetectionFilter ::
  DetectionFilter
mkDetectionFilter =
  DetectionFilter'
    { minBoundingBoxHeight = Core.Nothing,
      minBoundingBoxWidth = Core.Nothing,
      minConfidence = Core.Nothing
    }

-- | Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
--
-- /Note:/ Consider using 'minBoundingBoxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinBoundingBoxHeight :: Lens.Lens' DetectionFilter (Core.Maybe Core.Double)
dfMinBoundingBoxHeight = Lens.field @"minBoundingBoxHeight"
{-# DEPRECATED dfMinBoundingBoxHeight "Use generic-lens or generic-optics with 'minBoundingBoxHeight' instead." #-}

-- | Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
--
-- /Note:/ Consider using 'minBoundingBoxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinBoundingBoxWidth :: Lens.Lens' DetectionFilter (Core.Maybe Core.Double)
dfMinBoundingBoxWidth = Lens.field @"minBoundingBoxWidth"
{-# DEPRECATED dfMinBoundingBoxWidth "Use generic-lens or generic-optics with 'minBoundingBoxWidth' instead." #-}

-- | Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinConfidence :: Lens.Lens' DetectionFilter (Core.Maybe Core.Double)
dfMinConfidence = Lens.field @"minConfidence"
{-# DEPRECATED dfMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

instance Core.FromJSON DetectionFilter where
  toJSON DetectionFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("MinBoundingBoxHeight" Core..=) Core.<$> minBoundingBoxHeight,
            ("MinBoundingBoxWidth" Core..=) Core.<$> minBoundingBoxWidth,
            ("MinConfidence" Core..=) Core.<$> minConfidence
          ]
      )
