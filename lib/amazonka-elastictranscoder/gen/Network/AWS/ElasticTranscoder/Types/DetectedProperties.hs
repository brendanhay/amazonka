{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.DetectedProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.DetectedProperties
  ( DetectedProperties (..),

    -- * Smart constructor
    mkDetectedProperties,

    -- * Lenses
    dpDurationMillis,
    dpFileSize,
    dpFrameRate,
    dpHeight,
    dpWidth,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.FrameRate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detected properties of the input file. Elastic Transcoder identifies these values from the input file.
--
-- /See:/ 'mkDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { -- | The detected duration of the input file, in milliseconds.
    durationMillis :: Core.Maybe Core.Integer,
    -- | The detected file size of the input file, in bytes.
    fileSize :: Core.Maybe Core.Integer,
    -- | The detected frame rate of the input file, in frames per second.
    frameRate :: Core.Maybe Types.FrameRate,
    -- | The detected height of the input file, in pixels.
    height :: Core.Maybe Core.Int,
    -- | The detected width of the input file, in pixels.
    width :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectedProperties' value with any optional fields omitted.
mkDetectedProperties ::
  DetectedProperties
mkDetectedProperties =
  DetectedProperties'
    { durationMillis = Core.Nothing,
      fileSize = Core.Nothing,
      frameRate = Core.Nothing,
      height = Core.Nothing,
      width = Core.Nothing
    }

-- | The detected duration of the input file, in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDurationMillis :: Lens.Lens' DetectedProperties (Core.Maybe Core.Integer)
dpDurationMillis = Lens.field @"durationMillis"
{-# DEPRECATED dpDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | The detected file size of the input file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFileSize :: Lens.Lens' DetectedProperties (Core.Maybe Core.Integer)
dpFileSize = Lens.field @"fileSize"
{-# DEPRECATED dpFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | The detected frame rate of the input file, in frames per second.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFrameRate :: Lens.Lens' DetectedProperties (Core.Maybe Types.FrameRate)
dpFrameRate = Lens.field @"frameRate"
{-# DEPRECATED dpFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | The detected height of the input file, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpHeight :: Lens.Lens' DetectedProperties (Core.Maybe Core.Int)
dpHeight = Lens.field @"height"
{-# DEPRECATED dpHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | The detected width of the input file, in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpWidth :: Lens.Lens' DetectedProperties (Core.Maybe Core.Int)
dpWidth = Lens.field @"width"
{-# DEPRECATED dpWidth "Use generic-lens or generic-optics with 'width' instead." #-}

instance Core.FromJSON DetectedProperties where
  toJSON DetectedProperties {..} =
    Core.object
      ( Core.catMaybes
          [ ("DurationMillis" Core..=) Core.<$> durationMillis,
            ("FileSize" Core..=) Core.<$> fileSize,
            ("FrameRate" Core..=) Core.<$> frameRate,
            ("Height" Core..=) Core.<$> height,
            ("Width" Core..=) Core.<$> width
          ]
      )

instance Core.FromJSON DetectedProperties where
  parseJSON =
    Core.withObject "DetectedProperties" Core.$
      \x ->
        DetectedProperties'
          Core.<$> (x Core..:? "DurationMillis")
          Core.<*> (x Core..:? "FileSize")
          Core.<*> (x Core..:? "FrameRate")
          Core.<*> (x Core..:? "Height")
          Core.<*> (x Core..:? "Width")
