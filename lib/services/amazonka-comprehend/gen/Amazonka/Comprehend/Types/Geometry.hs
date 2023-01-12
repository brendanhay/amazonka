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
-- Module      : Amazonka.Comprehend.Types.Geometry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Geometry where

import Amazonka.Comprehend.Types.BoundingBox
import Amazonka.Comprehend.Types.Point
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of items on a document page.
--
-- For additional information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/API_Geometry.html Geometry>
-- in the Amazon Textract API reference.
--
-- /See:/ 'newGeometry' smart constructor.
data Geometry = Geometry'
  { -- | An axis-aligned coarse representation of the location of the recognized
    -- item on the document page.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Within the bounding box, a fine-grained polygon around the recognized
    -- item.
    polygon :: Prelude.Maybe [Point]
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
-- 'boundingBox', 'geometry_boundingBox' - An axis-aligned coarse representation of the location of the recognized
-- item on the document page.
--
-- 'polygon', 'geometry_polygon' - Within the bounding box, a fine-grained polygon around the recognized
-- item.
newGeometry ::
  Geometry
newGeometry =
  Geometry'
    { boundingBox = Prelude.Nothing,
      polygon = Prelude.Nothing
    }

-- | An axis-aligned coarse representation of the location of the recognized
-- item on the document page.
geometry_boundingBox :: Lens.Lens' Geometry (Prelude.Maybe BoundingBox)
geometry_boundingBox = Lens.lens (\Geometry' {boundingBox} -> boundingBox) (\s@Geometry' {} a -> s {boundingBox = a} :: Geometry)

-- | Within the bounding box, a fine-grained polygon around the recognized
-- item.
geometry_polygon :: Lens.Lens' Geometry (Prelude.Maybe [Point])
geometry_polygon = Lens.lens (\Geometry' {polygon} -> polygon) (\s@Geometry' {} a -> s {polygon = a} :: Geometry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Geometry where
  parseJSON =
    Data.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Prelude.<$> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Polygon" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Geometry where
  hashWithSalt _salt Geometry' {..} =
    _salt `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` polygon

instance Prelude.NFData Geometry where
  rnf Geometry' {..} =
    Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf polygon
