{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Landmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Landmark
  ( Landmark (..)
  -- * Smart constructor
  , mkLandmark
  -- * Lenses
  , lType
  , lX
  , lY
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.LandmarkType as Types

-- | Indicates the location of the landmark on the face.
--
-- /See:/ 'mkLandmark' smart constructor.
data Landmark = Landmark'
  { type' :: Core.Maybe Types.LandmarkType
    -- ^ Type of landmark.
  , x :: Core.Maybe Core.Double
    -- ^ The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5. 
  , y :: Core.Maybe Core.Double
    -- ^ The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Landmark' value with any optional fields omitted.
mkLandmark
    :: Landmark
mkLandmark
  = Landmark'{type' = Core.Nothing, x = Core.Nothing,
              y = Core.Nothing}

-- | Type of landmark.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Landmark (Core.Maybe Types.LandmarkType)
lType = Lens.field @"type'"
{-# INLINEABLE lType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The x-coordinate of the landmark expressed as a ratio of the width of the image. The x-coordinate is measured from the left-side of the image. For example, if the image is 700 pixels wide and the x-coordinate of the landmark is at 350 pixels, this value is 0.5. 
--
-- /Note:/ Consider using 'x' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lX :: Lens.Lens' Landmark (Core.Maybe Core.Double)
lX = Lens.field @"x"
{-# INLINEABLE lX #-}
{-# DEPRECATED x "Use generic-lens or generic-optics with 'x' instead"  #-}

-- | The y-coordinate of the landmark expressed as a ratio of the height of the image. The y-coordinate is measured from the top of the image. For example, if the image height is 200 pixels and the y-coordinate of the landmark is at 50 pixels, this value is 0.25.
--
-- /Note:/ Consider using 'y' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lY :: Lens.Lens' Landmark (Core.Maybe Core.Double)
lY = Lens.field @"y"
{-# INLINEABLE lY #-}
{-# DEPRECATED y "Use generic-lens or generic-optics with 'y' instead"  #-}

instance Core.FromJSON Landmark where
        parseJSON
          = Core.withObject "Landmark" Core.$
              \ x ->
                Landmark' Core.<$>
                  (x Core..:? "Type") Core.<*> x Core..:? "X" Core.<*> x Core..:? "Y"
