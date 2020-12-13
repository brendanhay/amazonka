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
    ssdfTechnicalCueFilter,
    ssdfShotFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.StartShotDetectionFilter
import Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter

-- | Filters applied to the technical cue or shot detection segments. For more information, see 'StartSegmentDetection' .
--
-- /See:/ 'mkStartSegmentDetectionFilters' smart constructor.
data StartSegmentDetectionFilters = StartSegmentDetectionFilters'
  { -- | Filters that are specific to technical cues.
    technicalCueFilter :: Lude.Maybe StartTechnicalCueDetectionFilter,
    -- | Filters that are specific to shot detections.
    shotFilter :: Lude.Maybe StartShotDetectionFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSegmentDetectionFilters' with the minimum fields required to make a request.
--
-- * 'technicalCueFilter' - Filters that are specific to technical cues.
-- * 'shotFilter' - Filters that are specific to shot detections.
mkStartSegmentDetectionFilters ::
  StartSegmentDetectionFilters
mkStartSegmentDetectionFilters =
  StartSegmentDetectionFilters'
    { technicalCueFilter = Lude.Nothing,
      shotFilter = Lude.Nothing
    }

-- | Filters that are specific to technical cues.
--
-- /Note:/ Consider using 'technicalCueFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfTechnicalCueFilter :: Lens.Lens' StartSegmentDetectionFilters (Lude.Maybe StartTechnicalCueDetectionFilter)
ssdfTechnicalCueFilter = Lens.lens (technicalCueFilter :: StartSegmentDetectionFilters -> Lude.Maybe StartTechnicalCueDetectionFilter) (\s a -> s {technicalCueFilter = a} :: StartSegmentDetectionFilters)
{-# DEPRECATED ssdfTechnicalCueFilter "Use generic-lens or generic-optics with 'technicalCueFilter' instead." #-}

-- | Filters that are specific to shot detections.
--
-- /Note:/ Consider using 'shotFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdfShotFilter :: Lens.Lens' StartSegmentDetectionFilters (Lude.Maybe StartShotDetectionFilter)
ssdfShotFilter = Lens.lens (shotFilter :: StartSegmentDetectionFilters -> Lude.Maybe StartShotDetectionFilter) (\s a -> s {shotFilter = a} :: StartSegmentDetectionFilters)
{-# DEPRECATED ssdfShotFilter "Use generic-lens or generic-optics with 'shotFilter' instead." #-}

instance Lude.ToJSON StartSegmentDetectionFilters where
  toJSON StartSegmentDetectionFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TechnicalCueFilter" Lude..=) Lude.<$> technicalCueFilter,
            ("ShotFilter" Lude..=) Lude.<$> shotFilter
          ]
      )
