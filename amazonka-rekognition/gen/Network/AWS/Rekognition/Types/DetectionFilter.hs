{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.DetectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A set of parameters that allow you to filter out certain results from
-- your returned results.
--
-- /See:/ 'newDetectionFilter' smart constructor.
data DetectionFilter = DetectionFilter'
  { -- | Sets the minimum width of the word bounding box. Words with bounding
    -- boxes widths lesser than this value will be excluded from the result.
    -- Value is relative to the video frame width.
    minBoundingBoxWidth :: Prelude.Maybe Prelude.Double,
    -- | Sets confidence of word detection. Words with detection confidence below
    -- this will be excluded from the result. Values should be between 0.5 and
    -- 1 as Text in Video will not return any result below 0.5.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | Sets the minimum height of the word bounding box. Words with bounding
    -- box heights lesser than this value will be excluded from the result.
    -- Value is relative to the video frame height.
    minBoundingBoxHeight :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minBoundingBoxWidth', 'detectionFilter_minBoundingBoxWidth' - Sets the minimum width of the word bounding box. Words with bounding
-- boxes widths lesser than this value will be excluded from the result.
-- Value is relative to the video frame width.
--
-- 'minConfidence', 'detectionFilter_minConfidence' - Sets confidence of word detection. Words with detection confidence below
-- this will be excluded from the result. Values should be between 0.5 and
-- 1 as Text in Video will not return any result below 0.5.
--
-- 'minBoundingBoxHeight', 'detectionFilter_minBoundingBoxHeight' - Sets the minimum height of the word bounding box. Words with bounding
-- box heights lesser than this value will be excluded from the result.
-- Value is relative to the video frame height.
newDetectionFilter ::
  DetectionFilter
newDetectionFilter =
  DetectionFilter'
    { minBoundingBoxWidth =
        Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      minBoundingBoxHeight = Prelude.Nothing
    }

-- | Sets the minimum width of the word bounding box. Words with bounding
-- boxes widths lesser than this value will be excluded from the result.
-- Value is relative to the video frame width.
detectionFilter_minBoundingBoxWidth :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minBoundingBoxWidth = Lens.lens (\DetectionFilter' {minBoundingBoxWidth} -> minBoundingBoxWidth) (\s@DetectionFilter' {} a -> s {minBoundingBoxWidth = a} :: DetectionFilter)

-- | Sets confidence of word detection. Words with detection confidence below
-- this will be excluded from the result. Values should be between 0.5 and
-- 1 as Text in Video will not return any result below 0.5.
detectionFilter_minConfidence :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minConfidence = Lens.lens (\DetectionFilter' {minConfidence} -> minConfidence) (\s@DetectionFilter' {} a -> s {minConfidence = a} :: DetectionFilter)

-- | Sets the minimum height of the word bounding box. Words with bounding
-- box heights lesser than this value will be excluded from the result.
-- Value is relative to the video frame height.
detectionFilter_minBoundingBoxHeight :: Lens.Lens' DetectionFilter (Prelude.Maybe Prelude.Double)
detectionFilter_minBoundingBoxHeight = Lens.lens (\DetectionFilter' {minBoundingBoxHeight} -> minBoundingBoxHeight) (\s@DetectionFilter' {} a -> s {minBoundingBoxHeight = a} :: DetectionFilter)

instance Prelude.Hashable DetectionFilter

instance Prelude.NFData DetectionFilter

instance Prelude.ToJSON DetectionFilter where
  toJSON DetectionFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MinBoundingBoxWidth" Prelude..=)
              Prelude.<$> minBoundingBoxWidth,
            ("MinConfidence" Prelude..=)
              Prelude.<$> minConfidence,
            ("MinBoundingBoxHeight" Prelude..=)
              Prelude.<$> minBoundingBoxHeight
          ]
      )
