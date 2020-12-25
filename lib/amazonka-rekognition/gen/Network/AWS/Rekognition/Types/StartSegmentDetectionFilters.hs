{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
  ( StartSegmentDetectionFilters (..),

    -- * Smart constructor
    mkStartSegmentDetectionFilters,

    -- * Lenses
    ssdfShotFilter,
    ssdfTechnicalCueFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.StartShotDetectionFilter as Types
import qualified Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter as Types

-- | Filters applied to the technical cue or shot detection segments. For more information, see 'StartSegmentDetection' .
--
-- /See:/ 'mkStartSegmentDetectionFilters' smart constructor.
data StartSegmentDetectionFilters = StartSegmentDetectionFilters'
  { -- | Filters that are specific to shot detections.
    shotFilter :: Core.Maybe Types.StartShotDetectionFilter,
    -- | Filters that are specific to technical cues.
    technicalCueFilter :: Core.Maybe Types.StartTechnicalCueDetectionFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSegmentDetectionFilters' value with any optional fields omitted.
mkStartSegmentDetectionFilters ::
  StartSegmentDetectionFilters
mkStartSegmentDetectionFilters =
  StartSegmentDetectionFilters'
    { shotFilter = Core.Nothing,
      technicalCueFilter = Core.Nothing
    }

-- | Filters that are specific to shot detections.
--
-- /Note:/ Consider using 'shotFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfShotFilter :: Lens.Lens' StartSegmentDetectionFilters (Core.Maybe Types.StartShotDetectionFilter)
ssdfShotFilter = Lens.field @"shotFilter"
{-# DEPRECATED ssdfShotFilter "Use generic-lens or generic-optics with 'shotFilter' instead." #-}

-- | Filters that are specific to technical cues.
--
-- /Note:/ Consider using 'technicalCueFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfTechnicalCueFilter :: Lens.Lens' StartSegmentDetectionFilters (Core.Maybe Types.StartTechnicalCueDetectionFilter)
ssdfTechnicalCueFilter = Lens.field @"technicalCueFilter"
{-# DEPRECATED ssdfTechnicalCueFilter "Use generic-lens or generic-optics with 'technicalCueFilter' instead." #-}

instance Core.FromJSON StartSegmentDetectionFilters where
  toJSON StartSegmentDetectionFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShotFilter" Core..=) Core.<$> shotFilter,
            ("TechnicalCueFilter" Core..=) Core.<$> technicalCueFilter
          ]
      )
