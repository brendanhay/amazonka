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
import qualified Network.AWS.Prelude as Lude

-- | Specify the offset between the upper-left corner of the video frame and the top left corner of the overlay.
--
-- /See:/ 'mkMotionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { imageX ::
      Lude.Maybe Lude.Natural,
    imageY :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MotionImageInsertionOffset' with the minimum fields required to make a request.
--
-- * 'imageX' - Set the distance, in pixels, between the overlay and the left edge of the video frame.
-- * 'imageY' - Set the distance, in pixels, between the overlay and the top edge of the video frame.
mkMotionImageInsertionOffset ::
  MotionImageInsertionOffset
mkMotionImageInsertionOffset =
  MotionImageInsertionOffset'
    { imageX = Lude.Nothing,
      imageY = Lude.Nothing
    }

-- | Set the distance, in pixels, between the overlay and the left edge of the video frame.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miioImageX :: Lens.Lens' MotionImageInsertionOffset (Lude.Maybe Lude.Natural)
miioImageX = Lens.lens (imageX :: MotionImageInsertionOffset -> Lude.Maybe Lude.Natural) (\s a -> s {imageX = a} :: MotionImageInsertionOffset)
{-# DEPRECATED miioImageX "Use generic-lens or generic-optics with 'imageX' instead." #-}

-- | Set the distance, in pixels, between the overlay and the top edge of the video frame.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miioImageY :: Lens.Lens' MotionImageInsertionOffset (Lude.Maybe Lude.Natural)
miioImageY = Lens.lens (imageY :: MotionImageInsertionOffset -> Lude.Maybe Lude.Natural) (\s a -> s {imageY = a} :: MotionImageInsertionOffset)
{-# DEPRECATED miioImageY "Use generic-lens or generic-optics with 'imageY' instead." #-}

instance Lude.FromJSON MotionImageInsertionOffset where
  parseJSON =
    Lude.withObject
      "MotionImageInsertionOffset"
      ( \x ->
          MotionImageInsertionOffset'
            Lude.<$> (x Lude..:? "imageX") Lude.<*> (x Lude..:? "imageY")
      )

instance Lude.ToJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageX" Lude..=) Lude.<$> imageX,
            ("imageY" Lude..=) Lude.<$> imageY
          ]
      )
