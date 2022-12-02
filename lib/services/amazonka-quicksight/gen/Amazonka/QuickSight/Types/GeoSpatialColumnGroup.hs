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
-- Module      : Amazonka.QuickSight.Types.GeoSpatialColumnGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeoSpatialColumnGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeoSpatialCountryCode

-- | Geospatial column group that denotes a hierarchy.
--
-- /See:/ 'newGeoSpatialColumnGroup' smart constructor.
data GeoSpatialColumnGroup = GeoSpatialColumnGroup'
  { -- | Country code.
    countryCode :: Prelude.Maybe GeoSpatialCountryCode,
    -- | A display name for the hierarchy.
    name :: Prelude.Text,
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
-- 'countryCode', 'geoSpatialColumnGroup_countryCode' - Country code.
--
-- 'name', 'geoSpatialColumnGroup_name' - A display name for the hierarchy.
--
-- 'columns', 'geoSpatialColumnGroup_columns' - Columns in this hierarchy.
newGeoSpatialColumnGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'columns'
  Prelude.NonEmpty Prelude.Text ->
  GeoSpatialColumnGroup
newGeoSpatialColumnGroup pName_ pColumns_ =
  GeoSpatialColumnGroup'
    { countryCode =
        Prelude.Nothing,
      name = pName_,
      columns = Lens.coerced Lens.# pColumns_
    }

-- | Country code.
geoSpatialColumnGroup_countryCode :: Lens.Lens' GeoSpatialColumnGroup (Prelude.Maybe GeoSpatialCountryCode)
geoSpatialColumnGroup_countryCode = Lens.lens (\GeoSpatialColumnGroup' {countryCode} -> countryCode) (\s@GeoSpatialColumnGroup' {} a -> s {countryCode = a} :: GeoSpatialColumnGroup)

-- | A display name for the hierarchy.
geoSpatialColumnGroup_name :: Lens.Lens' GeoSpatialColumnGroup Prelude.Text
geoSpatialColumnGroup_name = Lens.lens (\GeoSpatialColumnGroup' {name} -> name) (\s@GeoSpatialColumnGroup' {} a -> s {name = a} :: GeoSpatialColumnGroup)

-- | Columns in this hierarchy.
geoSpatialColumnGroup_columns :: Lens.Lens' GeoSpatialColumnGroup (Prelude.NonEmpty Prelude.Text)
geoSpatialColumnGroup_columns = Lens.lens (\GeoSpatialColumnGroup' {columns} -> columns) (\s@GeoSpatialColumnGroup' {} a -> s {columns = a} :: GeoSpatialColumnGroup) Prelude.. Lens.coerced

instance Data.FromJSON GeoSpatialColumnGroup where
  parseJSON =
    Data.withObject
      "GeoSpatialColumnGroup"
      ( \x ->
          GeoSpatialColumnGroup'
            Prelude.<$> (x Data..:? "CountryCode")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Columns")
      )

instance Prelude.Hashable GeoSpatialColumnGroup where
  hashWithSalt _salt GeoSpatialColumnGroup' {..} =
    _salt `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` columns

instance Prelude.NFData GeoSpatialColumnGroup where
  rnf GeoSpatialColumnGroup' {..} =
    Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf columns

instance Data.ToJSON GeoSpatialColumnGroup where
  toJSON GeoSpatialColumnGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CountryCode" Data..=) Prelude.<$> countryCode,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Columns" Data..= columns)
          ]
      )
