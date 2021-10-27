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
-- Module      : Network.AWS.QuickSight.Types.GeoSpatialColumnGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.GeoSpatialColumnGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.GeoSpatialCountryCode

-- | Geospatial column group that denotes a hierarchy.
--
-- /See:/ 'newGeoSpatialColumnGroup' smart constructor.
data GeoSpatialColumnGroup = GeoSpatialColumnGroup'
  { -- | A display name for the hierarchy.
    name :: Prelude.Text,
    -- | Country code.
    countryCode :: GeoSpatialCountryCode,
    -- | Columns in this hierarchy.
    columns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoSpatialColumnGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'geoSpatialColumnGroup_name' - A display name for the hierarchy.
--
-- 'countryCode', 'geoSpatialColumnGroup_countryCode' - Country code.
--
-- 'columns', 'geoSpatialColumnGroup_columns' - Columns in this hierarchy.
newGeoSpatialColumnGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'countryCode'
  GeoSpatialCountryCode ->
  -- | 'columns'
  Prelude.NonEmpty Prelude.Text ->
  GeoSpatialColumnGroup
newGeoSpatialColumnGroup
  pName_
  pCountryCode_
  pColumns_ =
    GeoSpatialColumnGroup'
      { name = pName_,
        countryCode = pCountryCode_,
        columns = Lens.coerced Lens.# pColumns_
      }

-- | A display name for the hierarchy.
geoSpatialColumnGroup_name :: Lens.Lens' GeoSpatialColumnGroup Prelude.Text
geoSpatialColumnGroup_name = Lens.lens (\GeoSpatialColumnGroup' {name} -> name) (\s@GeoSpatialColumnGroup' {} a -> s {name = a} :: GeoSpatialColumnGroup)

-- | Country code.
geoSpatialColumnGroup_countryCode :: Lens.Lens' GeoSpatialColumnGroup GeoSpatialCountryCode
geoSpatialColumnGroup_countryCode = Lens.lens (\GeoSpatialColumnGroup' {countryCode} -> countryCode) (\s@GeoSpatialColumnGroup' {} a -> s {countryCode = a} :: GeoSpatialColumnGroup)

-- | Columns in this hierarchy.
geoSpatialColumnGroup_columns :: Lens.Lens' GeoSpatialColumnGroup (Prelude.NonEmpty Prelude.Text)
geoSpatialColumnGroup_columns = Lens.lens (\GeoSpatialColumnGroup' {columns} -> columns) (\s@GeoSpatialColumnGroup' {} a -> s {columns = a} :: GeoSpatialColumnGroup) Prelude.. Lens.coerced

instance Core.FromJSON GeoSpatialColumnGroup where
  parseJSON =
    Core.withObject
      "GeoSpatialColumnGroup"
      ( \x ->
          GeoSpatialColumnGroup'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "CountryCode")
            Prelude.<*> (x Core..: "Columns")
      )

instance Prelude.Hashable GeoSpatialColumnGroup

instance Prelude.NFData GeoSpatialColumnGroup

instance Core.ToJSON GeoSpatialColumnGroup where
  toJSON GeoSpatialColumnGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("CountryCode" Core..= countryCode),
            Prelude.Just ("Columns" Core..= columns)
          ]
      )
