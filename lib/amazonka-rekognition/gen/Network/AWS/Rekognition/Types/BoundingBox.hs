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
    bbWidth,
    bbTop,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the bounding box around the label, face, text or personal protective equipment. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
-- The @top@ and @left@ values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
--
-- /See:/ 'mkBoundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { height :: Lude.Maybe Lude.Double,
    left :: Lude.Maybe Lude.Double,
    width :: Lude.Maybe Lude.Double,
    top :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BoundingBox' with the minimum fields required to make a request.
--
-- * 'height' - Height of the bounding box as a ratio of the overall image height.
-- * 'left' - Left coordinate of the bounding box as a ratio of overall image width.
-- * 'top' - Top coordinate of the bounding box as a ratio of overall image height.
-- * 'width' - Width of the bounding box as a ratio of the overall image width.
mkBoundingBox ::
  BoundingBox
mkBoundingBox =
  BoundingBox'
    { height = Lude.Nothing,
      left = Lude.Nothing,
      width = Lude.Nothing,
      top = Lude.Nothing
    }

-- | Height of the bounding box as a ratio of the overall image height.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbHeight :: Lens.Lens' BoundingBox (Lude.Maybe Lude.Double)
bbHeight = Lens.lens (height :: BoundingBox -> Lude.Maybe Lude.Double) (\s a -> s {height = a} :: BoundingBox)
{-# DEPRECATED bbHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Left coordinate of the bounding box as a ratio of overall image width.
--
-- /Note:/ Consider using 'left' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbLeft :: Lens.Lens' BoundingBox (Lude.Maybe Lude.Double)
bbLeft = Lens.lens (left :: BoundingBox -> Lude.Maybe Lude.Double) (\s a -> s {left = a} :: BoundingBox)
{-# DEPRECATED bbLeft "Use generic-lens or generic-optics with 'left' instead." #-}

-- | Width of the bounding box as a ratio of the overall image width.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbWidth :: Lens.Lens' BoundingBox (Lude.Maybe Lude.Double)
bbWidth = Lens.lens (width :: BoundingBox -> Lude.Maybe Lude.Double) (\s a -> s {width = a} :: BoundingBox)
{-# DEPRECATED bbWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | Top coordinate of the bounding box as a ratio of overall image height.
--
-- /Note:/ Consider using 'top' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbTop :: Lens.Lens' BoundingBox (Lude.Maybe Lude.Double)
bbTop = Lens.lens (top :: BoundingBox -> Lude.Maybe Lude.Double) (\s a -> s {top = a} :: BoundingBox)
{-# DEPRECATED bbTop "Use generic-lens or generic-optics with 'top' instead." #-}

instance Lude.FromJSON BoundingBox where
  parseJSON =
    Lude.withObject
      "BoundingBox"
      ( \x ->
          BoundingBox'
            Lude.<$> (x Lude..:? "Height")
            Lude.<*> (x Lude..:? "Left")
            Lude.<*> (x Lude..:? "Width")
            Lude.<*> (x Lude..:? "Top")
      )

instance Lude.ToJSON BoundingBox where
  toJSON BoundingBox' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Height" Lude..=) Lude.<$> height,
            ("Left" Lude..=) Lude.<$> left,
            ("Width" Lude..=) Lude.<$> width,
            ("Top" Lude..=) Lude.<$> top
          ]
      )
