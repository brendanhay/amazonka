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
-- Module      : Network.AWS.Rekognition.Types.Geometry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Geometry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Point

-- | Information about where an object (DetectCustomLabels) or text
-- (DetectText) is located on an image.
--
-- /See:/ 'newGeometry' smart constructor.
data Geometry = Geometry'
  { -- | Within the bounding box, a fine-grained polygon around the detected
    -- item.
    polygon :: Core.Maybe [Point],
    -- | An axis-aligned coarse representation of the detected item\'s location
    -- on the image.
    boundingBox :: Core.Maybe BoundingBox
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Geometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polygon', 'geometry_polygon' - Within the bounding box, a fine-grained polygon around the detected
-- item.
--
-- 'boundingBox', 'geometry_boundingBox' - An axis-aligned coarse representation of the detected item\'s location
-- on the image.
newGeometry ::
  Geometry
newGeometry =
  Geometry'
    { polygon = Core.Nothing,
      boundingBox = Core.Nothing
    }

-- | Within the bounding box, a fine-grained polygon around the detected
-- item.
geometry_polygon :: Lens.Lens' Geometry (Core.Maybe [Point])
geometry_polygon = Lens.lens (\Geometry' {polygon} -> polygon) (\s@Geometry' {} a -> s {polygon = a} :: Geometry) Core.. Lens.mapping Lens._Coerce

-- | An axis-aligned coarse representation of the detected item\'s location
-- on the image.
geometry_boundingBox :: Lens.Lens' Geometry (Core.Maybe BoundingBox)
geometry_boundingBox = Lens.lens (\Geometry' {boundingBox} -> boundingBox) (\s@Geometry' {} a -> s {boundingBox = a} :: Geometry)

instance Core.FromJSON Geometry where
  parseJSON =
    Core.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Core.<$> (x Core..:? "Polygon" Core..!= Core.mempty)
            Core.<*> (x Core..:? "BoundingBox")
      )

instance Core.Hashable Geometry

instance Core.NFData Geometry
