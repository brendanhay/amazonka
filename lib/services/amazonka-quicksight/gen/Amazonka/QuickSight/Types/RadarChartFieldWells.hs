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
-- Module      : Amazonka.QuickSight.Types.RadarChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RadarChartAggregatedFieldWells

-- | The field wells of a radar chart visual.
--
-- /See:/ 'newRadarChartFieldWells' smart constructor.
data RadarChartFieldWells = RadarChartFieldWells'
  { -- | The aggregated field wells of a radar chart visual.
    radarChartAggregatedFieldWells :: Prelude.Maybe RadarChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'radarChartAggregatedFieldWells', 'radarChartFieldWells_radarChartAggregatedFieldWells' - The aggregated field wells of a radar chart visual.
newRadarChartFieldWells ::
  RadarChartFieldWells
newRadarChartFieldWells =
  RadarChartFieldWells'
    { radarChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a radar chart visual.
radarChartFieldWells_radarChartAggregatedFieldWells :: Lens.Lens' RadarChartFieldWells (Prelude.Maybe RadarChartAggregatedFieldWells)
radarChartFieldWells_radarChartAggregatedFieldWells = Lens.lens (\RadarChartFieldWells' {radarChartAggregatedFieldWells} -> radarChartAggregatedFieldWells) (\s@RadarChartFieldWells' {} a -> s {radarChartAggregatedFieldWells = a} :: RadarChartFieldWells)

instance Data.FromJSON RadarChartFieldWells where
  parseJSON =
    Data.withObject
      "RadarChartFieldWells"
      ( \x ->
          RadarChartFieldWells'
            Prelude.<$> (x Data..:? "RadarChartAggregatedFieldWells")
      )

instance Prelude.Hashable RadarChartFieldWells where
  hashWithSalt _salt RadarChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` radarChartAggregatedFieldWells

instance Prelude.NFData RadarChartFieldWells where
  rnf RadarChartFieldWells' {..} =
    Prelude.rnf radarChartAggregatedFieldWells

instance Data.ToJSON RadarChartFieldWells where
  toJSON RadarChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RadarChartAggregatedFieldWells" Data..=)
              Prelude.<$> radarChartAggregatedFieldWells
          ]
      )
