{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartSegmentDetectionFilters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.StartShotDetectionFilter
import Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter

-- | Filters applied to the technical cue or shot detection segments. For
-- more information, see StartSegmentDetection.
--
-- /See:/ 'newStartSegmentDetectionFilters' smart constructor.
data StartSegmentDetectionFilters = StartSegmentDetectionFilters'
  { -- | Filters that are specific to technical cues.
    technicalCueFilter :: Core.Maybe StartTechnicalCueDetectionFilter,
    -- | Filters that are specific to shot detections.
    shotFilter :: Core.Maybe StartShotDetectionFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSegmentDetectionFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'technicalCueFilter', 'startSegmentDetectionFilters_technicalCueFilter' - Filters that are specific to technical cues.
--
-- 'shotFilter', 'startSegmentDetectionFilters_shotFilter' - Filters that are specific to shot detections.
newStartSegmentDetectionFilters ::
  StartSegmentDetectionFilters
newStartSegmentDetectionFilters =
  StartSegmentDetectionFilters'
    { technicalCueFilter =
        Core.Nothing,
      shotFilter = Core.Nothing
    }

-- | Filters that are specific to technical cues.
startSegmentDetectionFilters_technicalCueFilter :: Lens.Lens' StartSegmentDetectionFilters (Core.Maybe StartTechnicalCueDetectionFilter)
startSegmentDetectionFilters_technicalCueFilter = Lens.lens (\StartSegmentDetectionFilters' {technicalCueFilter} -> technicalCueFilter) (\s@StartSegmentDetectionFilters' {} a -> s {technicalCueFilter = a} :: StartSegmentDetectionFilters)

-- | Filters that are specific to shot detections.
startSegmentDetectionFilters_shotFilter :: Lens.Lens' StartSegmentDetectionFilters (Core.Maybe StartShotDetectionFilter)
startSegmentDetectionFilters_shotFilter = Lens.lens (\StartSegmentDetectionFilters' {shotFilter} -> shotFilter) (\s@StartSegmentDetectionFilters' {} a -> s {shotFilter = a} :: StartSegmentDetectionFilters)

instance Core.Hashable StartSegmentDetectionFilters

instance Core.NFData StartSegmentDetectionFilters

instance Core.ToJSON StartSegmentDetectionFilters where
  toJSON StartSegmentDetectionFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TechnicalCueFilter" Core..=)
              Core.<$> technicalCueFilter,
            ("ShotFilter" Core..=) Core.<$> shotFilter
          ]
      )
