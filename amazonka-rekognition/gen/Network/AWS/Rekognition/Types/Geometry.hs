{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Point

-- | Information about where an object (DetectCustomLabels) or text
-- (DetectText) is located on an image.
--
-- /See:/ 'newGeometry' smart constructor.
data Geometry = Geometry'
  { -- | Within the bounding box, a fine-grained polygon around the detected
    -- item.
    polygon :: Prelude.Maybe [Point],
    -- | An axis-aligned coarse representation of the detected item\'s location
    -- on the image.
    boundingBox :: Prelude.Maybe BoundingBox
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { polygon = Prelude.Nothing,
      boundingBox = Prelude.Nothing
    }

-- | Within the bounding box, a fine-grained polygon around the detected
-- item.
geometry_polygon :: Lens.Lens' Geometry (Prelude.Maybe [Point])
geometry_polygon = Lens.lens (\Geometry' {polygon} -> polygon) (\s@Geometry' {} a -> s {polygon = a} :: Geometry) Prelude.. Lens.mapping Prelude._Coerce

-- | An axis-aligned coarse representation of the detected item\'s location
-- on the image.
geometry_boundingBox :: Lens.Lens' Geometry (Prelude.Maybe BoundingBox)
geometry_boundingBox = Lens.lens (\Geometry' {boundingBox} -> boundingBox) (\s@Geometry' {} a -> s {boundingBox = a} :: Geometry)

instance Prelude.FromJSON Geometry where
  parseJSON =
    Prelude.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Prelude.<$> (x Prelude..:? "Polygon" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "BoundingBox")
      )

instance Prelude.Hashable Geometry

instance Prelude.NFData Geometry
