{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.BoundingBox
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.BoundingBox
  ( BoundingBox (..),

    -- * Smart constructor
    mkBoundingBox,

    -- * Lenses
    bbHeight,
    bbLeft,
    bbTop,
    bbWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the bounding box around the label, face, text or personal protective equipment. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
-- The @top@ and @left@ values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
--
-- /See:/ 'mkBoundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { -- | Height of the bounding box as a ratio of the overall image height.
    height :: Core.Maybe Core.Double,
    -- | Left coordinate of the bounding box as a ratio of overall image width.
    left :: Core.Maybe Core.Double,
    -- | Top coordinate of the bounding box as a ratio of overall image height.
    top :: Core.Maybe Core.Double,
    -- | Width of the bounding box as a ratio of the overall image width.
    width :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BoundingBox' value with any optional fields omitted.
mkBoundingBox ::
  BoundingBox
mkBoundingBox =
  BoundingBox'
    { height = Core.Nothing,
      left = Core.Nothing,
      top = Core.Nothing,
      width = Core.Nothing
    }

-- | Height of the bounding box as a ratio of the overall image height.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbHeight :: Lens.Lens' BoundingBox (Core.Maybe Core.Double)
bbHeight = Lens.field @"height"
{-# DEPRECATED bbHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Left coordinate of the bounding box as a ratio of overall image width.
--
-- /Note:/ Consider using 'left' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbLeft :: Lens.Lens' BoundingBox (Core.Maybe Core.Double)
bbLeft = Lens.field @"left"
{-# DEPRECATED bbLeft "Use generic-lens or generic-optics with 'left' instead." #-}

-- | Top coordinate of the bounding box as a ratio of overall image height.
--
-- /Note:/ Consider using 'top' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbTop :: Lens.Lens' BoundingBox (Core.Maybe Core.Double)
bbTop = Lens.field @"top"
{-# DEPRECATED bbTop "Use generic-lens or generic-optics with 'top' instead." #-}

-- | Width of the bounding box as a ratio of the overall image width.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbWidth :: Lens.Lens' BoundingBox (Core.Maybe Core.Double)
bbWidth = Lens.field @"width"
{-# DEPRECATED bbWidth "Use generic-lens or generic-optics with 'width' instead." #-}

instance Core.FromJSON BoundingBox where
  toJSON BoundingBox {..} =
    Core.object
      ( Core.catMaybes
          [ ("Height" Core..=) Core.<$> height,
            ("Left" Core..=) Core.<$> left,
            ("Top" Core..=) Core.<$> top,
            ("Width" Core..=) Core.<$> width
          ]
      )

instance Core.FromJSON BoundingBox where
  parseJSON =
    Core.withObject "BoundingBox" Core.$
      \x ->
        BoundingBox'
          Core.<$> (x Core..:? "Height")
          Core.<*> (x Core..:? "Left")
          Core.<*> (x Core..:? "Top")
          Core.<*> (x Core..:? "Width")
