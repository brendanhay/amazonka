{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
  ( MotionImageInsertionOffset (..),

    -- * Smart constructor
    mkMotionImageInsertionOffset,

    -- * Lenses
    miioImageX,
    miioImageY,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specify the offset between the upper-left corner of the video frame and the top left corner of the overlay.
--
-- /See:/ 'mkMotionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { -- | Set the distance, in pixels, between the overlay and the left edge of the video frame.
    imageX :: Core.Maybe Core.Natural,
    -- | Set the distance, in pixels, between the overlay and the top edge of the video frame.
    imageY :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MotionImageInsertionOffset' value with any optional fields omitted.
mkMotionImageInsertionOffset ::
  MotionImageInsertionOffset
mkMotionImageInsertionOffset =
  MotionImageInsertionOffset'
    { imageX = Core.Nothing,
      imageY = Core.Nothing
    }

-- | Set the distance, in pixels, between the overlay and the left edge of the video frame.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miioImageX :: Lens.Lens' MotionImageInsertionOffset (Core.Maybe Core.Natural)
miioImageX = Lens.field @"imageX"
{-# DEPRECATED miioImageX "Use generic-lens or generic-optics with 'imageX' instead." #-}

-- | Set the distance, in pixels, between the overlay and the top edge of the video frame.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miioImageY :: Lens.Lens' MotionImageInsertionOffset (Core.Maybe Core.Natural)
miioImageY = Lens.field @"imageY"
{-# DEPRECATED miioImageY "Use generic-lens or generic-optics with 'imageY' instead." #-}

instance Core.FromJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset {..} =
    Core.object
      ( Core.catMaybes
          [ ("imageX" Core..=) Core.<$> imageX,
            ("imageY" Core..=) Core.<$> imageY
          ]
      )

instance Core.FromJSON MotionImageInsertionOffset where
  parseJSON =
    Core.withObject "MotionImageInsertionOffset" Core.$
      \x ->
        MotionImageInsertionOffset'
          Core.<$> (x Core..:? "imageX") Core.<*> (x Core..:? "imageY")
