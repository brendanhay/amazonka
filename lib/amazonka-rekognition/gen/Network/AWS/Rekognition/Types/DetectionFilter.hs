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
import qualified Network.AWS.Prelude as Lude

-- | A set of parameters that allow you to filter out certain results from your returned results.
--
-- /See:/ 'mkDetectionFilter' smart constructor.
data DetectionFilter = DetectionFilter'
  { -- | Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
    minBoundingBoxHeight :: Lude.Maybe Lude.Double,
    -- | Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
    minBoundingBoxWidth :: Lude.Maybe Lude.Double,
    -- | Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
    minConfidence :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectionFilter' with the minimum fields required to make a request.
--
-- * 'minBoundingBoxHeight' - Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
-- * 'minBoundingBoxWidth' - Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
-- * 'minConfidence' - Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
mkDetectionFilter ::
  DetectionFilter
mkDetectionFilter =
  DetectionFilter'
    { minBoundingBoxHeight = Lude.Nothing,
      minBoundingBoxWidth = Lude.Nothing,
      minConfidence = Lude.Nothing
    }

-- | Sets the minimum height of the word bounding box. Words with bounding box heights lesser than this value will be excluded from the result. Value is relative to the video frame height.
--
-- /Note:/ Consider using 'minBoundingBoxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinBoundingBoxHeight :: Lens.Lens' DetectionFilter (Lude.Maybe Lude.Double)
dfMinBoundingBoxHeight = Lens.lens (minBoundingBoxHeight :: DetectionFilter -> Lude.Maybe Lude.Double) (\s a -> s {minBoundingBoxHeight = a} :: DetectionFilter)
{-# DEPRECATED dfMinBoundingBoxHeight "Use generic-lens or generic-optics with 'minBoundingBoxHeight' instead." #-}

-- | Sets the minimum width of the word bounding box. Words with bounding boxes widths lesser than this value will be excluded from the result. Value is relative to the video frame width.
--
-- /Note:/ Consider using 'minBoundingBoxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinBoundingBoxWidth :: Lens.Lens' DetectionFilter (Lude.Maybe Lude.Double)
dfMinBoundingBoxWidth = Lens.lens (minBoundingBoxWidth :: DetectionFilter -> Lude.Maybe Lude.Double) (\s a -> s {minBoundingBoxWidth = a} :: DetectionFilter)
{-# DEPRECATED dfMinBoundingBoxWidth "Use generic-lens or generic-optics with 'minBoundingBoxWidth' instead." #-}

-- | Sets confidence of word detection. Words with detection confidence below this will be excluded from the result. Values should be between 0.5 and 1 as Text in Video will not return any result below 0.5.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfMinConfidence :: Lens.Lens' DetectionFilter (Lude.Maybe Lude.Double)
dfMinConfidence = Lens.lens (minConfidence :: DetectionFilter -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: DetectionFilter)
{-# DEPRECATED dfMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

instance Lude.ToJSON DetectionFilter where
  toJSON DetectionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MinBoundingBoxHeight" Lude..=) Lude.<$> minBoundingBoxHeight,
            ("MinBoundingBoxWidth" Lude..=) Lude.<$> minBoundingBoxWidth,
            ("MinConfidence" Lude..=) Lude.<$> minConfidence
          ]
      )
