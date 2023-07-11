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
-- Module      : Amazonka.QuickSight.Types.HeatMapFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeatMapFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.HeatMapAggregatedFieldWells

-- | The field well configuration of a heat map.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newHeatMapFieldWells' smart constructor.
data HeatMapFieldWells = HeatMapFieldWells'
  { -- | The aggregated field wells of a heat map.
    heatMapAggregatedFieldWells :: Prelude.Maybe HeatMapAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeatMapFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'heatMapAggregatedFieldWells', 'heatMapFieldWells_heatMapAggregatedFieldWells' - The aggregated field wells of a heat map.
newHeatMapFieldWells ::
  HeatMapFieldWells
newHeatMapFieldWells =
  HeatMapFieldWells'
    { heatMapAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a heat map.
heatMapFieldWells_heatMapAggregatedFieldWells :: Lens.Lens' HeatMapFieldWells (Prelude.Maybe HeatMapAggregatedFieldWells)
heatMapFieldWells_heatMapAggregatedFieldWells = Lens.lens (\HeatMapFieldWells' {heatMapAggregatedFieldWells} -> heatMapAggregatedFieldWells) (\s@HeatMapFieldWells' {} a -> s {heatMapAggregatedFieldWells = a} :: HeatMapFieldWells)

instance Data.FromJSON HeatMapFieldWells where
  parseJSON =
    Data.withObject
      "HeatMapFieldWells"
      ( \x ->
          HeatMapFieldWells'
            Prelude.<$> (x Data..:? "HeatMapAggregatedFieldWells")
      )

instance Prelude.Hashable HeatMapFieldWells where
  hashWithSalt _salt HeatMapFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` heatMapAggregatedFieldWells

instance Prelude.NFData HeatMapFieldWells where
  rnf HeatMapFieldWells' {..} =
    Prelude.rnf heatMapAggregatedFieldWells

instance Data.ToJSON HeatMapFieldWells where
  toJSON HeatMapFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HeatMapAggregatedFieldWells" Data..=)
              Prelude.<$> heatMapAggregatedFieldWells
          ]
      )
