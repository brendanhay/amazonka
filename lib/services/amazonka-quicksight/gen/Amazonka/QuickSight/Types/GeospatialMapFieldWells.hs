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
-- Module      : Amazonka.QuickSight.Types.GeospatialMapFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialMapFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeospatialMapAggregatedFieldWells

-- | The field wells of a @GeospatialMapVisual@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newGeospatialMapFieldWells' smart constructor.
data GeospatialMapFieldWells = GeospatialMapFieldWells'
  { -- | The aggregated field well for a geospatial map.
    geospatialMapAggregatedFieldWells :: Prelude.Maybe GeospatialMapAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialMapFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geospatialMapAggregatedFieldWells', 'geospatialMapFieldWells_geospatialMapAggregatedFieldWells' - The aggregated field well for a geospatial map.
newGeospatialMapFieldWells ::
  GeospatialMapFieldWells
newGeospatialMapFieldWells =
  GeospatialMapFieldWells'
    { geospatialMapAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field well for a geospatial map.
geospatialMapFieldWells_geospatialMapAggregatedFieldWells :: Lens.Lens' GeospatialMapFieldWells (Prelude.Maybe GeospatialMapAggregatedFieldWells)
geospatialMapFieldWells_geospatialMapAggregatedFieldWells = Lens.lens (\GeospatialMapFieldWells' {geospatialMapAggregatedFieldWells} -> geospatialMapAggregatedFieldWells) (\s@GeospatialMapFieldWells' {} a -> s {geospatialMapAggregatedFieldWells = a} :: GeospatialMapFieldWells)

instance Data.FromJSON GeospatialMapFieldWells where
  parseJSON =
    Data.withObject
      "GeospatialMapFieldWells"
      ( \x ->
          GeospatialMapFieldWells'
            Prelude.<$> (x Data..:? "GeospatialMapAggregatedFieldWells")
      )

instance Prelude.Hashable GeospatialMapFieldWells where
  hashWithSalt _salt GeospatialMapFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` geospatialMapAggregatedFieldWells

instance Prelude.NFData GeospatialMapFieldWells where
  rnf GeospatialMapFieldWells' {..} =
    Prelude.rnf geospatialMapAggregatedFieldWells

instance Data.ToJSON GeospatialMapFieldWells where
  toJSON GeospatialMapFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GeospatialMapAggregatedFieldWells" Data..=)
              Prelude.<$> geospatialMapAggregatedFieldWells
          ]
      )
