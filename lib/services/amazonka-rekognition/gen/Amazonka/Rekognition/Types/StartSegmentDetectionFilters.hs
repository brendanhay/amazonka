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
-- Module      : Amazonka.Rekognition.Types.StartSegmentDetectionFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StartSegmentDetectionFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.StartShotDetectionFilter
import Amazonka.Rekognition.Types.StartTechnicalCueDetectionFilter

-- | Filters applied to the technical cue or shot detection segments. For
-- more information, see StartSegmentDetection.
--
-- /See:/ 'newStartSegmentDetectionFilters' smart constructor.
data StartSegmentDetectionFilters = StartSegmentDetectionFilters'
  { -- | Filters that are specific to technical cues.
    technicalCueFilter :: Prelude.Maybe StartTechnicalCueDetectionFilter,
    -- | Filters that are specific to shot detections.
    shotFilter :: Prelude.Maybe StartShotDetectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      shotFilter = Prelude.Nothing
    }

-- | Filters that are specific to technical cues.
startSegmentDetectionFilters_technicalCueFilter :: Lens.Lens' StartSegmentDetectionFilters (Prelude.Maybe StartTechnicalCueDetectionFilter)
startSegmentDetectionFilters_technicalCueFilter = Lens.lens (\StartSegmentDetectionFilters' {technicalCueFilter} -> technicalCueFilter) (\s@StartSegmentDetectionFilters' {} a -> s {technicalCueFilter = a} :: StartSegmentDetectionFilters)

-- | Filters that are specific to shot detections.
startSegmentDetectionFilters_shotFilter :: Lens.Lens' StartSegmentDetectionFilters (Prelude.Maybe StartShotDetectionFilter)
startSegmentDetectionFilters_shotFilter = Lens.lens (\StartSegmentDetectionFilters' {shotFilter} -> shotFilter) (\s@StartSegmentDetectionFilters' {} a -> s {shotFilter = a} :: StartSegmentDetectionFilters)

instance
  Prelude.Hashable
    StartSegmentDetectionFilters
  where
  hashWithSalt _salt StartSegmentDetectionFilters' {..} =
    _salt `Prelude.hashWithSalt` technicalCueFilter
      `Prelude.hashWithSalt` shotFilter

instance Prelude.NFData StartSegmentDetectionFilters where
  rnf StartSegmentDetectionFilters' {..} =
    Prelude.rnf technicalCueFilter
      `Prelude.seq` Prelude.rnf shotFilter

instance Core.ToJSON StartSegmentDetectionFilters where
  toJSON StartSegmentDetectionFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TechnicalCueFilter" Core..=)
              Prelude.<$> technicalCueFilter,
            ("ShotFilter" Core..=) Prelude.<$> shotFilter
          ]
      )
