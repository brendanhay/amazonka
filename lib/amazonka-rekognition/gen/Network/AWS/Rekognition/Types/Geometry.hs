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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Point

-- | Information about where an object ('DetectCustomLabels' ) or text ('DetectText' ) is located on an image.
--
-- /See:/ 'mkGeometry' smart constructor.
data Geometry = Geometry'
  { boundingBox :: Lude.Maybe BoundingBox,
    polygon :: Lude.Maybe [Point]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Geometry' with the minimum fields required to make a request.
--
-- * 'boundingBox' - An axis-aligned coarse representation of the detected item's location on the image.
-- * 'polygon' - Within the bounding box, a fine-grained polygon around the detected item.
mkGeometry ::
  Geometry
mkGeometry =
  Geometry' {boundingBox = Lude.Nothing, polygon = Lude.Nothing}

-- | An axis-aligned coarse representation of the detected item's location on the image.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBoundingBox :: Lens.Lens' Geometry (Lude.Maybe BoundingBox)
gBoundingBox = Lens.lens (boundingBox :: Geometry -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: Geometry)
{-# DEPRECATED gBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Within the bounding box, a fine-grained polygon around the detected item.
--
-- /Note:/ Consider using 'polygon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPolygon :: Lens.Lens' Geometry (Lude.Maybe [Point])
gPolygon = Lens.lens (polygon :: Geometry -> Lude.Maybe [Point]) (\s a -> s {polygon = a} :: Geometry)
{-# DEPRECATED gPolygon "Use generic-lens or generic-optics with 'polygon' instead." #-}

instance Lude.FromJSON Geometry where
  parseJSON =
    Lude.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Lude.<$> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Polygon" Lude..!= Lude.mempty)
      )
