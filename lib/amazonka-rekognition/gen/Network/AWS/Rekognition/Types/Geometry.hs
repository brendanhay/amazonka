{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Geometry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Geometry
  ( Geometry (..),

    -- * Smart constructor
    mkGeometry,

    -- * Lenses
    gBoundingBox,
    gPolygon,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.Point as Types

-- | Information about where an object ('DetectCustomLabels' ) or text ('DetectText' ) is located on an image.
--
-- /See:/ 'mkGeometry' smart constructor.
data Geometry = Geometry'
  { -- | An axis-aligned coarse representation of the detected item's location on the image.
    boundingBox :: Core.Maybe Types.BoundingBox,
    -- | Within the bounding box, a fine-grained polygon around the detected item.
    polygon :: Core.Maybe [Types.Point]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Geometry' value with any optional fields omitted.
mkGeometry ::
  Geometry
mkGeometry =
  Geometry' {boundingBox = Core.Nothing, polygon = Core.Nothing}

-- | An axis-aligned coarse representation of the detected item's location on the image.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBoundingBox :: Lens.Lens' Geometry (Core.Maybe Types.BoundingBox)
gBoundingBox = Lens.field @"boundingBox"
{-# DEPRECATED gBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Within the bounding box, a fine-grained polygon around the detected item.
--
-- /Note:/ Consider using 'polygon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPolygon :: Lens.Lens' Geometry (Core.Maybe [Types.Point])
gPolygon = Lens.field @"polygon"
{-# DEPRECATED gPolygon "Use generic-lens or generic-optics with 'polygon' instead." #-}

instance Core.FromJSON Geometry where
  parseJSON =
    Core.withObject "Geometry" Core.$
      \x ->
        Geometry'
          Core.<$> (x Core..:? "BoundingBox") Core.<*> (x Core..:? "Polygon")
