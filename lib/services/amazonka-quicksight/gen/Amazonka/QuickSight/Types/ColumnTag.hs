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
-- Module      : Amazonka.QuickSight.Types.ColumnTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnDescription
import Amazonka.QuickSight.Types.GeoSpatialDataRole

-- | A tag for a column in a @ TagColumnOperation @ structure. This is a
-- variant type structure. For this structure to be valid, only one of the
-- attributes can be non-null.
--
-- /See:/ 'newColumnTag' smart constructor.
data ColumnTag = ColumnTag'
  { -- | A geospatial role for a column.
    columnGeographicRole :: Prelude.Maybe GeoSpatialDataRole,
    -- | A description for a column.
    columnDescription :: Prelude.Maybe ColumnDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnGeographicRole', 'columnTag_columnGeographicRole' - A geospatial role for a column.
--
-- 'columnDescription', 'columnTag_columnDescription' - A description for a column.
newColumnTag ::
  ColumnTag
newColumnTag =
  ColumnTag'
    { columnGeographicRole = Prelude.Nothing,
      columnDescription = Prelude.Nothing
    }

-- | A geospatial role for a column.
columnTag_columnGeographicRole :: Lens.Lens' ColumnTag (Prelude.Maybe GeoSpatialDataRole)
columnTag_columnGeographicRole = Lens.lens (\ColumnTag' {columnGeographicRole} -> columnGeographicRole) (\s@ColumnTag' {} a -> s {columnGeographicRole = a} :: ColumnTag)

-- | A description for a column.
columnTag_columnDescription :: Lens.Lens' ColumnTag (Prelude.Maybe ColumnDescription)
columnTag_columnDescription = Lens.lens (\ColumnTag' {columnDescription} -> columnDescription) (\s@ColumnTag' {} a -> s {columnDescription = a} :: ColumnTag)

instance Core.FromJSON ColumnTag where
  parseJSON =
    Core.withObject
      "ColumnTag"
      ( \x ->
          ColumnTag'
            Prelude.<$> (x Core..:? "ColumnGeographicRole")
            Prelude.<*> (x Core..:? "ColumnDescription")
      )

instance Prelude.Hashable ColumnTag where
  hashWithSalt _salt ColumnTag' {..} =
    _salt `Prelude.hashWithSalt` columnGeographicRole
      `Prelude.hashWithSalt` columnDescription

instance Prelude.NFData ColumnTag where
  rnf ColumnTag' {..} =
    Prelude.rnf columnGeographicRole
      `Prelude.seq` Prelude.rnf columnDescription

instance Core.ToJSON ColumnTag where
  toJSON ColumnTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ColumnGeographicRole" Core..=)
              Prelude.<$> columnGeographicRole,
            ("ColumnDescription" Core..=)
              Prelude.<$> columnDescription
          ]
      )
