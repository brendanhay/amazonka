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
-- Module      : Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Filters for the technical segments returned by GetSegmentDetection. For
-- more information, see StartSegmentDetectionFilters.
--
-- /See:/ 'newStartTechnicalCueDetectionFilter' smart constructor.
data StartTechnicalCueDetectionFilter = StartTechnicalCueDetectionFilter'
  { -- | Specifies the minimum confidence that Amazon Rekognition Video must have
    -- in order to return a detected segment. Confidence represents how certain
    -- Amazon Rekognition is that a segment is correctly identified. 0 is the
    -- lowest confidence. 100 is the highest confidence. Amazon Rekognition
    -- Video doesn\'t return any segments with a confidence level lower than
    -- this specified value.
    --
    -- If you don\'t specify @MinSegmentConfidence@, @GetSegmentDetection@
    -- returns segments with confidence values greater than or equal to 50
    -- percent.
    minSegmentConfidence :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTechnicalCueDetectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSegmentConfidence', 'startTechnicalCueDetectionFilter_minSegmentConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have
-- in order to return a detected segment. Confidence represents how certain
-- Amazon Rekognition is that a segment is correctly identified. 0 is the
-- lowest confidence. 100 is the highest confidence. Amazon Rekognition
-- Video doesn\'t return any segments with a confidence level lower than
-- this specified value.
--
-- If you don\'t specify @MinSegmentConfidence@, @GetSegmentDetection@
-- returns segments with confidence values greater than or equal to 50
-- percent.
newStartTechnicalCueDetectionFilter ::
  StartTechnicalCueDetectionFilter
newStartTechnicalCueDetectionFilter =
  StartTechnicalCueDetectionFilter'
    { minSegmentConfidence =
        Core.Nothing
    }

-- | Specifies the minimum confidence that Amazon Rekognition Video must have
-- in order to return a detected segment. Confidence represents how certain
-- Amazon Rekognition is that a segment is correctly identified. 0 is the
-- lowest confidence. 100 is the highest confidence. Amazon Rekognition
-- Video doesn\'t return any segments with a confidence level lower than
-- this specified value.
--
-- If you don\'t specify @MinSegmentConfidence@, @GetSegmentDetection@
-- returns segments with confidence values greater than or equal to 50
-- percent.
startTechnicalCueDetectionFilter_minSegmentConfidence :: Lens.Lens' StartTechnicalCueDetectionFilter (Core.Maybe Core.Double)
startTechnicalCueDetectionFilter_minSegmentConfidence = Lens.lens (\StartTechnicalCueDetectionFilter' {minSegmentConfidence} -> minSegmentConfidence) (\s@StartTechnicalCueDetectionFilter' {} a -> s {minSegmentConfidence = a} :: StartTechnicalCueDetectionFilter)

instance
  Core.Hashable
    StartTechnicalCueDetectionFilter

instance Core.NFData StartTechnicalCueDetectionFilter

instance Core.ToJSON StartTechnicalCueDetectionFilter where
  toJSON StartTechnicalCueDetectionFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MinSegmentConfidence" Core..=)
              Core.<$> minSegmentConfidence
          ]
      )
