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
-- Module      : Amazonka.Rekognition.Types.StartTechnicalCueDetectionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StartTechnicalCueDetectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BlackFrame

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
    minSegmentConfidence :: Prelude.Maybe Prelude.Double,
    -- | A filter that allows you to control the black frame detection by
    -- specifying the black levels and pixel coverage of black pixels in a
    -- frame. Videos can come from multiple sources, formats, and time periods,
    -- with different standards and varying noise levels for black frames that
    -- need to be accounted for.
    blackFrame :: Prelude.Maybe BlackFrame
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- 'blackFrame', 'startTechnicalCueDetectionFilter_blackFrame' - A filter that allows you to control the black frame detection by
-- specifying the black levels and pixel coverage of black pixels in a
-- frame. Videos can come from multiple sources, formats, and time periods,
-- with different standards and varying noise levels for black frames that
-- need to be accounted for.
newStartTechnicalCueDetectionFilter ::
  StartTechnicalCueDetectionFilter
newStartTechnicalCueDetectionFilter =
  StartTechnicalCueDetectionFilter'
    { minSegmentConfidence =
        Prelude.Nothing,
      blackFrame = Prelude.Nothing
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
startTechnicalCueDetectionFilter_minSegmentConfidence :: Lens.Lens' StartTechnicalCueDetectionFilter (Prelude.Maybe Prelude.Double)
startTechnicalCueDetectionFilter_minSegmentConfidence = Lens.lens (\StartTechnicalCueDetectionFilter' {minSegmentConfidence} -> minSegmentConfidence) (\s@StartTechnicalCueDetectionFilter' {} a -> s {minSegmentConfidence = a} :: StartTechnicalCueDetectionFilter)

-- | A filter that allows you to control the black frame detection by
-- specifying the black levels and pixel coverage of black pixels in a
-- frame. Videos can come from multiple sources, formats, and time periods,
-- with different standards and varying noise levels for black frames that
-- need to be accounted for.
startTechnicalCueDetectionFilter_blackFrame :: Lens.Lens' StartTechnicalCueDetectionFilter (Prelude.Maybe BlackFrame)
startTechnicalCueDetectionFilter_blackFrame = Lens.lens (\StartTechnicalCueDetectionFilter' {blackFrame} -> blackFrame) (\s@StartTechnicalCueDetectionFilter' {} a -> s {blackFrame = a} :: StartTechnicalCueDetectionFilter)

instance
  Prelude.Hashable
    StartTechnicalCueDetectionFilter
  where
  hashWithSalt
    _salt
    StartTechnicalCueDetectionFilter' {..} =
      _salt `Prelude.hashWithSalt` minSegmentConfidence
        `Prelude.hashWithSalt` blackFrame

instance
  Prelude.NFData
    StartTechnicalCueDetectionFilter
  where
  rnf StartTechnicalCueDetectionFilter' {..} =
    Prelude.rnf minSegmentConfidence
      `Prelude.seq` Prelude.rnf blackFrame

instance Core.ToJSON StartTechnicalCueDetectionFilter where
  toJSON StartTechnicalCueDetectionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinSegmentConfidence" Core..=)
              Prelude.<$> minSegmentConfidence,
            ("BlackFrame" Core..=) Prelude.<$> blackFrame
          ]
      )
