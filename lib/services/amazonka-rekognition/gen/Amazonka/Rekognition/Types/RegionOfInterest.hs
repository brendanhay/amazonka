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
-- Module      : Amazonka.Rekognition.Types.RegionOfInterest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.RegionOfInterest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.Point

-- | Specifies a location within the frame that Rekognition checks for
-- objects of interest such as text, labels, or faces. It uses a
-- @BoundingBox@ or @Polygon@ to set a region of the screen.
--
-- A word, face, or label is included in the region if it is more than half
-- in that region. If there is more than one region, the word, face, or
-- label is compared with all regions of the screen. Any object of interest
-- that is more than half in a region is kept in the results.
--
-- /See:/ 'newRegionOfInterest' smart constructor.
data RegionOfInterest = RegionOfInterest'
  { -- | The box representing a region of interest on screen.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Specifies a shape made up of up to 10 @Point@ objects to define a region
    -- of interest.
    polygon :: Prelude.Maybe [Point]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionOfInterest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'regionOfInterest_boundingBox' - The box representing a region of interest on screen.
--
-- 'polygon', 'regionOfInterest_polygon' - Specifies a shape made up of up to 10 @Point@ objects to define a region
-- of interest.
newRegionOfInterest ::
  RegionOfInterest
newRegionOfInterest =
  RegionOfInterest'
    { boundingBox = Prelude.Nothing,
      polygon = Prelude.Nothing
    }

-- | The box representing a region of interest on screen.
regionOfInterest_boundingBox :: Lens.Lens' RegionOfInterest (Prelude.Maybe BoundingBox)
regionOfInterest_boundingBox = Lens.lens (\RegionOfInterest' {boundingBox} -> boundingBox) (\s@RegionOfInterest' {} a -> s {boundingBox = a} :: RegionOfInterest)

-- | Specifies a shape made up of up to 10 @Point@ objects to define a region
-- of interest.
regionOfInterest_polygon :: Lens.Lens' RegionOfInterest (Prelude.Maybe [Point])
regionOfInterest_polygon = Lens.lens (\RegionOfInterest' {polygon} -> polygon) (\s@RegionOfInterest' {} a -> s {polygon = a} :: RegionOfInterest) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RegionOfInterest where
  parseJSON =
    Data.withObject
      "RegionOfInterest"
      ( \x ->
          RegionOfInterest'
            Prelude.<$> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Polygon" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RegionOfInterest where
  hashWithSalt _salt RegionOfInterest' {..} =
    _salt
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` polygon

instance Prelude.NFData RegionOfInterest where
  rnf RegionOfInterest' {..} =
    Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf polygon

instance Data.ToJSON RegionOfInterest where
  toJSON RegionOfInterest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BoundingBox" Data..=) Prelude.<$> boundingBox,
            ("Polygon" Data..=) Prelude.<$> polygon
          ]
      )
