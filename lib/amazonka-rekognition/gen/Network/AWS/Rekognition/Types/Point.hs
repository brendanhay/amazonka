{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Point
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Point
  ( Point (..)
  -- * Smart constructor
  , mkPoint
  -- * Lenses
  , pX
  , pY
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.
--
-- An array of @Point@ objects, @Polygon@ , is returned by 'DetectText' and by 'DetectCustomLabels' . @Polygon@ represents a fine-grained polygon around a detected item. For more information, see Geometry in the Amazon Rekognition Developer Guide. 
--
-- /See:/ 'mkPoint' smart constructor.
data Point = Point'
  { x :: Core.Maybe Core.Double
    -- ^ The value of the X coordinate for a point on a @Polygon@ .
  , y :: Core.Maybe Core.Double
    -- ^ The value of the Y coordinate for a point on a @Polygon@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Point' value with any optional fields omitted.
mkPoint
    :: Point
mkPoint = Point'{x = Core.Nothing, y = Core.Nothing}

-- | The value of the X coordinate for a point on a @Polygon@ .
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pX :: Lens.Lens' Point (Core.Maybe Core.Double)
pX = Lens.field @"x"
{-# INLINEABLE pX #-}
{-# DEPRECATED x "Use generic-lens or generic-optics with 'x' instead"  #-}

-- | The value of the Y coordinate for a point on a @Polygon@ .
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pY :: Lens.Lens' Point (Core.Maybe Core.Double)
pY = Lens.field @"y"
{-# INLINEABLE pY #-}
{-# DEPRECATED y "Use generic-lens or generic-optics with 'y' instead"  #-}

instance Core.FromJSON Point where
        parseJSON
          = Core.withObject "Point" Core.$
              \ x -> Point' Core.<$> (x Core..:? "X") Core.<*> x Core..:? "Y"
