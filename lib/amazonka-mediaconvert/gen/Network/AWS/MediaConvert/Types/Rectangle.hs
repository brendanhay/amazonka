{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Rectangle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Rectangle
  ( Rectangle (..)
  -- * Smart constructor
  , mkRectangle
  -- * Lenses
  , rHeight
  , rWidth
  , rX
  , rY
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'mkRectangle' smart constructor.
data Rectangle = Rectangle'
  { height :: Core.Maybe Core.Natural
    -- ^ Height of rectangle in pixels. Specify only even numbers.
  , width :: Core.Maybe Core.Natural
    -- ^ Width of rectangle in pixels. Specify only even numbers.
  , x :: Core.Maybe Core.Natural
    -- ^ The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
  , y :: Core.Maybe Core.Natural
    -- ^ The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rectangle' value with any optional fields omitted.
mkRectangle
    :: Rectangle
mkRectangle
  = Rectangle'{height = Core.Nothing, width = Core.Nothing,
               x = Core.Nothing, y = Core.Nothing}

-- | Height of rectangle in pixels. Specify only even numbers.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHeight :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rHeight = Lens.field @"height"
{-# INLINEABLE rHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | Width of rectangle in pixels. Specify only even numbers.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWidth :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rWidth = Lens.field @"width"
{-# INLINEABLE rWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

-- | The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rX :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rX = Lens.field @"x"
{-# INLINEABLE rX #-}
{-# DEPRECATED x "Use generic-lens or generic-optics with 'x' instead"  #-}

-- | The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rY :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rY = Lens.field @"y"
{-# INLINEABLE rY #-}
{-# DEPRECATED y "Use generic-lens or generic-optics with 'y' instead"  #-}

instance Core.FromJSON Rectangle where
        toJSON Rectangle{..}
          = Core.object
              (Core.catMaybes
                 [("height" Core..=) Core.<$> height,
                  ("width" Core..=) Core.<$> width, ("x" Core..=) Core.<$> x,
                  ("y" Core..=) Core.<$> y])

instance Core.FromJSON Rectangle where
        parseJSON
          = Core.withObject "Rectangle" Core.$
              \ x ->
                Rectangle' Core.<$>
                  (x Core..:? "height") Core.<*> x Core..:? "width" Core.<*>
                    x Core..:? "x"
                    Core.<*> x Core..:? "y"
