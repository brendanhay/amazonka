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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.TemporalStatisticsConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.TemporalStatisticsConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.GroupBy
import Amazonka.SageMakerGeoSpatial.Types.TemporalStatistics

-- | The structure representing the configuration for Temporal Statistics
-- operation.
--
-- /See:/ 'newTemporalStatisticsConfigInput' smart constructor.
data TemporalStatisticsConfigInput = TemporalStatisticsConfigInput'
  { -- | The input for the temporal statistics grouping by time frequency option.
    groupBy :: Prelude.Maybe GroupBy,
    -- | The list of target band names for the temporal statistic to calculate.
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of the statistics method options.
    statistics :: Prelude.NonEmpty TemporalStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemporalStatisticsConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupBy', 'temporalStatisticsConfigInput_groupBy' - The input for the temporal statistics grouping by time frequency option.
--
-- 'targetBands', 'temporalStatisticsConfigInput_targetBands' - The list of target band names for the temporal statistic to calculate.
--
-- 'statistics', 'temporalStatisticsConfigInput_statistics' - The list of the statistics method options.
newTemporalStatisticsConfigInput ::
  -- | 'statistics'
  Prelude.NonEmpty TemporalStatistics ->
  TemporalStatisticsConfigInput
newTemporalStatisticsConfigInput pStatistics_ =
  TemporalStatisticsConfigInput'
    { groupBy =
        Prelude.Nothing,
      targetBands = Prelude.Nothing,
      statistics =
        Lens.coerced Lens.# pStatistics_
    }

-- | The input for the temporal statistics grouping by time frequency option.
temporalStatisticsConfigInput_groupBy :: Lens.Lens' TemporalStatisticsConfigInput (Prelude.Maybe GroupBy)
temporalStatisticsConfigInput_groupBy = Lens.lens (\TemporalStatisticsConfigInput' {groupBy} -> groupBy) (\s@TemporalStatisticsConfigInput' {} a -> s {groupBy = a} :: TemporalStatisticsConfigInput)

-- | The list of target band names for the temporal statistic to calculate.
temporalStatisticsConfigInput_targetBands :: Lens.Lens' TemporalStatisticsConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
temporalStatisticsConfigInput_targetBands = Lens.lens (\TemporalStatisticsConfigInput' {targetBands} -> targetBands) (\s@TemporalStatisticsConfigInput' {} a -> s {targetBands = a} :: TemporalStatisticsConfigInput) Prelude.. Lens.mapping Lens.coerced

-- | The list of the statistics method options.
temporalStatisticsConfigInput_statistics :: Lens.Lens' TemporalStatisticsConfigInput (Prelude.NonEmpty TemporalStatistics)
temporalStatisticsConfigInput_statistics = Lens.lens (\TemporalStatisticsConfigInput' {statistics} -> statistics) (\s@TemporalStatisticsConfigInput' {} a -> s {statistics = a} :: TemporalStatisticsConfigInput) Prelude.. Lens.coerced

instance Data.FromJSON TemporalStatisticsConfigInput where
  parseJSON =
    Data.withObject
      "TemporalStatisticsConfigInput"
      ( \x ->
          TemporalStatisticsConfigInput'
            Prelude.<$> (x Data..:? "GroupBy")
            Prelude.<*> (x Data..:? "TargetBands")
            Prelude.<*> (x Data..: "Statistics")
      )

instance
  Prelude.Hashable
    TemporalStatisticsConfigInput
  where
  hashWithSalt _salt TemporalStatisticsConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` targetBands
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData TemporalStatisticsConfigInput where
  rnf TemporalStatisticsConfigInput' {..} =
    Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf targetBands
      `Prelude.seq` Prelude.rnf statistics

instance Data.ToJSON TemporalStatisticsConfigInput where
  toJSON TemporalStatisticsConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("TargetBands" Data..=) Prelude.<$> targetBands,
            Prelude.Just ("Statistics" Data..= statistics)
          ]
      )
