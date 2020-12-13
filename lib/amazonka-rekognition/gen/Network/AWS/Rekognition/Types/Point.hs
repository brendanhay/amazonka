{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Point
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Point
  ( Point (..),

    -- * Smart constructor
    mkPoint,

    -- * Lenses
    pX,
    pY,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.
--
-- An array of @Point@ objects, @Polygon@ , is returned by 'DetectText' and by 'DetectCustomLabels' . @Polygon@ represents a fine-grained polygon around a detected item. For more information, see Geometry in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkPoint' smart constructor.
data Point = Point'
  { -- | The value of the X coordinate for a point on a @Polygon@ .
    x :: Lude.Maybe Lude.Double,
    -- | The value of the Y coordinate for a point on a @Polygon@ .
    y :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Point' with the minimum fields required to make a request.
--
-- * 'x' - The value of the X coordinate for a point on a @Polygon@ .
-- * 'y' - The value of the Y coordinate for a point on a @Polygon@ .
mkPoint ::
  Point
mkPoint = Point' {x = Lude.Nothing, y = Lude.Nothing}

-- | The value of the X coordinate for a point on a @Polygon@ .
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pX :: Lens.Lens' Point (Lude.Maybe Lude.Double)
pX = Lens.lens (x :: Point -> Lude.Maybe Lude.Double) (\s a -> s {x = a} :: Point)
{-# DEPRECATED pX "Use generic-lens or generic-optics with 'x' instead." #-}

-- | The value of the Y coordinate for a point on a @Polygon@ .
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pY :: Lens.Lens' Point (Lude.Maybe Lude.Double)
pY = Lens.lens (y :: Point -> Lude.Maybe Lude.Double) (\s a -> s {y = a} :: Point)
{-# DEPRECATED pY "Use generic-lens or generic-optics with 'y' instead." #-}

instance Lude.FromJSON Point where
  parseJSON =
    Lude.withObject
      "Point"
      (\x -> Point' Lude.<$> (x Lude..:? "X") Lude.<*> (x Lude..:? "Y"))
