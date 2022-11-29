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
-- Module      : Amazonka.Rekognition.Types.DetectionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of parameters that allow you to filter out certain results from
-- your returned results.
--
-- /See:/ 'newDetectionFilter' smart constructor.
data DetectionFilter = DetectionFilter'
  { -- | Sets the minimum height of the word bounding box. Words with bounding
    -- box heights lesser than this value will be excluded from the result.
    -- Value is relative to the video frame height.
    minBoundingBoxHeight :: Prelude.Maybe Prelude.Double,
    -- | Sets the minimum width of the word bounding box. Words with bounding
    -- boxes widths lesser than this value will be excluded from the result.
    -- Value is relative to the video frame width.
    minBoundingBoxWidth :: Prelude.Maybe Prelude.Double,
    -- | Sets the confidence of word detection. Words with detection confidence
    -- below this will be excluded from the result. Values should be between 0
    -- and 100. The default MinConfidence is 80.
    minConfidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minBoundingBoxHeight', 'detectionFilter_minBoundingBoxHeight' - Sets the minimum height of the word bounding box. Words with bounding
-- box heights lesser than this value will be excluded from the result.
-- Value is relative to the video frame height.
--
-- 'minBoundingBoxWidth', 'detectionFilter_minBoundingBoxWidth' - Sets the minimum width of the word bounding box. Words with bounding
-- boxes widths lesser than this value will be excluded from the result.
-- Value is relative to the video frame width.
--
-- 'minConfidence', 'detectionFilter_minConfidence' - Sets the confidence of word detection. Words with detection confidence
-- below this will be excluded from the result. Values should be between 0
-- and 100. The default MinConfidence is 80.
newDetectionFilter ::
  DetectionFilter
newDetectionFilter =
  DetectionFilter'
    { minBoundingBoxHeight =
        Prelude.Nothing,
      minBoundingBoxWidth = Prelude.Nothing,
      minConfidence = Prelude.Nothing
    }

-- | Sets the minimum height of the word bounding box. Words with bounding
-- box heights lesser than this value will be excluded from the result.
-- Value is relative to the video frame height.
detectionFilter_minBoundingBoxHeight :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minBoundingBoxHeight = Lens.lens (\DetectionFilter' {minBoundingBoxHeight} -> minBoundingBoxHeight) (\s@DetectionFilter' {} a -> s {minBoundingBoxHeight = a} :: DetectionFilter)

-- | Sets the minimum width of the word bounding box. Words with bounding
-- boxes widths lesser than this value will be excluded from the result.
-- Value is relative to the video frame width.
detectionFilter_minBoundingBoxWidth :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minBoundingBoxWidth = Lens.lens (\DetectionFilter' {minBoundingBoxWidth} -> minBoundingBoxWidth) (\s@DetectionFilter' {} a -> s {minBoundingBoxWidth = a} :: DetectionFilter)

-- | Sets the confidence of word detection. Words with detection confidence
-- below this will be excluded from the result. Values should be between 0
-- and 100. The default MinConfidence is 80.
detectionFilter_minConfidence :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minConfidence = Lens.lens (\DetectionFilter' {minConfidence} -> minConfidence) (\s@DetectionFilter' {} a -> s {minConfidence = a} :: DetectionFilter)

instance Prelude.Hashable DetectionFilter where
  hashWithSalt _salt DetectionFilter' {..} =
    _salt `Prelude.hashWithSalt` minBoundingBoxHeight
      `Prelude.hashWithSalt` minBoundingBoxWidth
      `Prelude.hashWithSalt` minConfidence

instance Prelude.NFData DetectionFilter where
  rnf DetectionFilter' {..} =
    Prelude.rnf minBoundingBoxHeight
      `Prelude.seq` Prelude.rnf minBoundingBoxWidth
      `Prelude.seq` Prelude.rnf minConfidence

instance Core.ToJSON DetectionFilter where
  toJSON DetectionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinBoundingBoxHeight" Core..=)
              Prelude.<$> minBoundingBoxHeight,
            ("MinBoundingBoxWidth" Core..=)
              Prelude.<$> minBoundingBoxWidth,
            ("MinConfidence" Core..=) Prelude.<$> minConfidence
          ]
      )
