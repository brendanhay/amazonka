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
-- Module      : Amazonka.QuickSight.Types.ColumnGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeoSpatialColumnGroup

-- | Groupings of columns that work together in certain Amazon QuickSight
-- features. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
--
-- /See:/ 'newColumnGroup' smart constructor.
data ColumnGroup = ColumnGroup'
  { -- | Geospatial column group that denotes a hierarchy.
    geoSpatialColumnGroup :: Prelude.Maybe GeoSpatialColumnGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoSpatialColumnGroup', 'columnGroup_geoSpatialColumnGroup' - Geospatial column group that denotes a hierarchy.
newColumnGroup ::
  ColumnGroup
newColumnGroup =
  ColumnGroup'
    { geoSpatialColumnGroup =
        Prelude.Nothing
    }

-- | Geospatial column group that denotes a hierarchy.
columnGroup_geoSpatialColumnGroup :: Lens.Lens' ColumnGroup (Prelude.Maybe GeoSpatialColumnGroup)
columnGroup_geoSpatialColumnGroup = Lens.lens (\ColumnGroup' {geoSpatialColumnGroup} -> geoSpatialColumnGroup) (\s@ColumnGroup' {} a -> s {geoSpatialColumnGroup = a} :: ColumnGroup)

instance Core.FromJSON ColumnGroup where
  parseJSON =
    Core.withObject
      "ColumnGroup"
      ( \x ->
          ColumnGroup'
            Prelude.<$> (x Core..:? "GeoSpatialColumnGroup")
      )

instance Prelude.Hashable ColumnGroup where
  hashWithSalt _salt ColumnGroup' {..} =
    _salt `Prelude.hashWithSalt` geoSpatialColumnGroup

instance Prelude.NFData ColumnGroup where
  rnf ColumnGroup' {..} =
    Prelude.rnf geoSpatialColumnGroup

instance Core.ToJSON ColumnGroup where
  toJSON ColumnGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GeoSpatialColumnGroup" Core..=)
              Prelude.<$> geoSpatialColumnGroup
          ]
      )
