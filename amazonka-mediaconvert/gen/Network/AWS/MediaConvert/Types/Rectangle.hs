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
-- Module      : Network.AWS.MediaConvert.Types.Rectangle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Rectangle where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'newRectangle' smart constructor.
data Rectangle = Rectangle'
  { -- | Height of rectangle in pixels. Specify only even numbers.
    height :: Core.Maybe Core.Natural,
    -- | The distance, in pixels, between the rectangle and the top edge of the
    -- video frame. Specify only even numbers.
    y :: Core.Maybe Core.Natural,
    -- | Width of rectangle in pixels. Specify only even numbers.
    width :: Core.Maybe Core.Natural,
    -- | The distance, in pixels, between the rectangle and the left edge of the
    -- video frame. Specify only even numbers.
    x :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Rectangle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'rectangle_height' - Height of rectangle in pixels. Specify only even numbers.
--
-- 'y', 'rectangle_y' - The distance, in pixels, between the rectangle and the top edge of the
-- video frame. Specify only even numbers.
--
-- 'width', 'rectangle_width' - Width of rectangle in pixels. Specify only even numbers.
--
-- 'x', 'rectangle_x' - The distance, in pixels, between the rectangle and the left edge of the
-- video frame. Specify only even numbers.
newRectangle ::
  Rectangle
newRectangle =
  Rectangle'
    { height = Core.Nothing,
      y = Core.Nothing,
      width = Core.Nothing,
      x = Core.Nothing
    }

-- | Height of rectangle in pixels. Specify only even numbers.
rectangle_height :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rectangle_height = Lens.lens (\Rectangle' {height} -> height) (\s@Rectangle' {} a -> s {height = a} :: Rectangle)

-- | The distance, in pixels, between the rectangle and the top edge of the
-- video frame. Specify only even numbers.
rectangle_y :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rectangle_y = Lens.lens (\Rectangle' {y} -> y) (\s@Rectangle' {} a -> s {y = a} :: Rectangle)

-- | Width of rectangle in pixels. Specify only even numbers.
rectangle_width :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rectangle_width = Lens.lens (\Rectangle' {width} -> width) (\s@Rectangle' {} a -> s {width = a} :: Rectangle)

-- | The distance, in pixels, between the rectangle and the left edge of the
-- video frame. Specify only even numbers.
rectangle_x :: Lens.Lens' Rectangle (Core.Maybe Core.Natural)
rectangle_x = Lens.lens (\Rectangle' {x} -> x) (\s@Rectangle' {} a -> s {x = a} :: Rectangle)

instance Core.FromJSON Rectangle where
  parseJSON =
    Core.withObject
      "Rectangle"
      ( \x ->
          Rectangle'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "y")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "x")
      )

instance Core.Hashable Rectangle

instance Core.NFData Rectangle

instance Core.ToJSON Rectangle where
  toJSON Rectangle' {..} =
    Core.object
      ( Core.catMaybes
          [ ("height" Core..=) Core.<$> height,
            ("y" Core..=) Core.<$> y,
            ("width" Core..=) Core.<$> width,
            ("x" Core..=) Core.<$> x
          ]
      )
