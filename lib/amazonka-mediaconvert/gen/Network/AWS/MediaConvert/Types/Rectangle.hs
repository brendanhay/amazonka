{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Rectangle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Rectangle
  ( Rectangle (..),

    -- * Smart constructor
    mkRectangle,

    -- * Lenses
    rHeight,
    rWidth,
    rX,
    rY,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'mkRectangle' smart constructor.
data Rectangle = Rectangle'
  { -- | Height of rectangle in pixels. Specify only even numbers.
    height :: Lude.Maybe Lude.Natural,
    -- | Width of rectangle in pixels. Specify only even numbers.
    width :: Lude.Maybe Lude.Natural,
    -- | The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
    x :: Lude.Maybe Lude.Natural,
    -- | The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
    y :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rectangle' with the minimum fields required to make a request.
--
-- * 'height' - Height of rectangle in pixels. Specify only even numbers.
-- * 'width' - Width of rectangle in pixels. Specify only even numbers.
-- * 'x' - The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
-- * 'y' - The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
mkRectangle ::
  Rectangle
mkRectangle =
  Rectangle'
    { height = Lude.Nothing,
      width = Lude.Nothing,
      x = Lude.Nothing,
      y = Lude.Nothing
    }

-- | Height of rectangle in pixels. Specify only even numbers.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHeight :: Lens.Lens' Rectangle (Lude.Maybe Lude.Natural)
rHeight = Lens.lens (height :: Rectangle -> Lude.Maybe Lude.Natural) (\s a -> s {height = a} :: Rectangle)
{-# DEPRECATED rHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Width of rectangle in pixels. Specify only even numbers.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWidth :: Lens.Lens' Rectangle (Lude.Maybe Lude.Natural)
rWidth = Lens.lens (width :: Rectangle -> Lude.Maybe Lude.Natural) (\s a -> s {width = a} :: Rectangle)
{-# DEPRECATED rWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rX :: Lens.Lens' Rectangle (Lude.Maybe Lude.Natural)
rX = Lens.lens (x :: Rectangle -> Lude.Maybe Lude.Natural) (\s a -> s {x = a} :: Rectangle)
{-# DEPRECATED rX "Use generic-lens or generic-optics with 'x' instead." #-}

-- | The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rY :: Lens.Lens' Rectangle (Lude.Maybe Lude.Natural)
rY = Lens.lens (y :: Rectangle -> Lude.Maybe Lude.Natural) (\s a -> s {y = a} :: Rectangle)
{-# DEPRECATED rY "Use generic-lens or generic-optics with 'y' instead." #-}

instance Lude.FromJSON Rectangle where
  parseJSON =
    Lude.withObject
      "Rectangle"
      ( \x ->
          Rectangle'
            Lude.<$> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "x")
            Lude.<*> (x Lude..:? "y")
      )

instance Lude.ToJSON Rectangle where
  toJSON Rectangle' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("height" Lude..=) Lude.<$> height,
            ("width" Lude..=) Lude.<$> width,
            ("x" Lude..=) Lude.<$> x,
            ("y" Lude..=) Lude.<$> y
          ]
      )
