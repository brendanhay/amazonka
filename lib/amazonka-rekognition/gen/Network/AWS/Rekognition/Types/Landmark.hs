{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Landmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Landmark
  ( Landmark (..),

    -- * Smart constructor
    mkLandmark,

    -- * Lenses
    lType,
    lX,
    lY,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.LandmarkType

-- | Indicates the location of the landmark on the face.
--
-- /See:/ 'mkLandmark' smart constructor.
data Landmark = Landmark'
  { type' :: Lude.Maybe LandmarkType,
    x :: Lude.Maybe Lude.Double,
    y :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Landmark' with the minimum fields required to make a request.
--
-- * 'type'' - Type of landmark.
-- * 'x' - The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
-- * 'y' - The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
mkLandmark ::
  Landmark
mkLandmark =
  Landmark'
    { type' = Lude.Nothing,
      x = Lude.Nothing,
      y = Lude.Nothing
    }

-- | Type of landmark.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Landmark (Lude.Maybe LandmarkType)
lType = Lens.lens (type' :: Landmark -> Lude.Maybe LandmarkType) (\s a -> s {type' = a} :: Landmark)
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lX :: Lens.Lens' Landmark (Lude.Maybe Lude.Double)
lX = Lens.lens (x :: Landmark -> Lude.Maybe Lude.Double) (\s a -> s {x = a} :: Landmark)
{-# DEPRECATED lX "Use generic-lens or generic-optics with 'x' instead." #-}

-- | The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lY :: Lens.Lens' Landmark (Lude.Maybe Lude.Double)
lY = Lens.lens (y :: Landmark -> Lude.Maybe Lude.Double) (\s a -> s {y = a} :: Landmark)
{-# DEPRECATED lY "Use generic-lens or generic-optics with 'y' instead." #-}

instance Lude.FromJSON Landmark where
  parseJSON =
    Lude.withObject
      "Landmark"
      ( \x ->
          Landmark'
            Lude.<$> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "X")
            Lude.<*> (x Lude..:? "Y")
      )
