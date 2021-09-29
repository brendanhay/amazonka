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
-- Module      : Network.AWS.Textract.Types.Geometry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Types.Geometry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Textract.Types.BoundingBox
import Network.AWS.Textract.Types.Point

-- | Information about where the following items are located on a document
-- page: detected page, text, key-value pairs, tables, table cells, and
-- selection elements.
--
-- /See:/ 'newGeometry' smart constructor.
data Geometry = Geometry'
  { -- | Within the bounding box, a fine-grained polygon around the recognized
    -- item.
    polygon :: Prelude.Maybe [Point],
    -- | An axis-aligned coarse representation of the location of the recognized
    -- item on the document page.
    boundingBox :: Prelude.Maybe BoundingBox
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Geometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polygon', 'geometry_polygon' - Within the bounding box, a fine-grained polygon around the recognized
-- item.
--
-- 'boundingBox', 'geometry_boundingBox' - An axis-aligned coarse representation of the location of the recognized
-- item on the document page.
newGeometry ::
  Geometry
newGeometry =
  Geometry'
    { polygon = Prelude.Nothing,
      boundingBox = Prelude.Nothing
    }

-- | Within the bounding box, a fine-grained polygon around the recognized
-- item.
geometry_polygon :: Lens.Lens' Geometry (Prelude.Maybe [Point])
geometry_polygon = Lens.lens (\Geometry' {polygon} -> polygon) (\s@Geometry' {} a -> s {polygon = a} :: Geometry) Prelude.. Lens.mapping Lens._Coerce

-- | An axis-aligned coarse representation of the location of the recognized
-- item on the document page.
geometry_boundingBox :: Lens.Lens' Geometry (Prelude.Maybe BoundingBox)
geometry_boundingBox = Lens.lens (\Geometry' {boundingBox} -> boundingBox) (\s@Geometry' {} a -> s {boundingBox = a} :: Geometry)

instance Core.FromJSON Geometry where
  parseJSON =
    Core.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Prelude.<$> (x Core..:? "Polygon" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BoundingBox")
      )

instance Prelude.Hashable Geometry

instance Prelude.NFData Geometry
