{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CustomLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.CustomLabel
  ( CustomLabel (..)
  -- * Smart constructor
  , mkCustomLabel
  -- * Lenses
  , clConfidence
  , clGeometry
  , clName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Geometry as Types

-- | A custom label detected in an image by a call to 'DetectCustomLabels' .
--
-- /See:/ 'mkCustomLabel' smart constructor.
data CustomLabel = CustomLabel'
  { confidence :: Core.Maybe Core.Double
    -- ^ The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
  , geometry :: Core.Maybe Types.Geometry
    -- ^ The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the custom label.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomLabel' value with any optional fields omitted.
mkCustomLabel
    :: CustomLabel
mkCustomLabel
  = CustomLabel'{confidence = Core.Nothing, geometry = Core.Nothing,
                 name = Core.Nothing}

-- | The confidence that the model has in the detection of the custom label. The range is 0-100. A higher value indicates a higher confidence.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConfidence :: Lens.Lens' CustomLabel (Core.Maybe Core.Double)
clConfidence = Lens.field @"confidence"
{-# INLINEABLE clConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | The location of the detected object on the image that corresponds to the custom label. Includes an axis aligned coarse bounding box surrounding the object and a finer grain polygon for more accurate spatial information.
--
-- /Note:/ Consider using 'geometry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clGeometry :: Lens.Lens' CustomLabel (Core.Maybe Types.Geometry)
clGeometry = Lens.field @"geometry"
{-# INLINEABLE clGeometry #-}
{-# DEPRECATED geometry "Use generic-lens or generic-optics with 'geometry' instead"  #-}

-- | The name of the custom label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clName :: Lens.Lens' CustomLabel (Core.Maybe Core.Text)
clName = Lens.field @"name"
{-# INLINEABLE clName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON CustomLabel where
        parseJSON
          = Core.withObject "CustomLabel" Core.$
              \ x ->
                CustomLabel' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Geometry" Core.<*>
                    x Core..:? "Name"
