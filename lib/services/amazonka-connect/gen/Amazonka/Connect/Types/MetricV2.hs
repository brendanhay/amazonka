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
-- Module      : Amazonka.Connect.Types.MetricV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.MetricV2 where

import Amazonka.Connect.Types.MetricFilterV2
import Amazonka.Connect.Types.ThresholdV2
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the metric.
--
-- /See:/ 'newMetricV2' smart constructor.
data MetricV2 = MetricV2'
  { -- | Contains the filters to be used when returning data.
    metricFilters :: Prelude.Maybe [MetricFilterV2],
    -- | The name of the metric.
    --
    -- This parameter is required. The following Required = No is incorrect.
    name :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the threshold for service level metrics.
    threshold :: Prelude.Maybe [ThresholdV2]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricFilters', 'metricV2_metricFilters' - Contains the filters to be used when returning data.
--
-- 'name', 'metricV2_name' - The name of the metric.
--
-- This parameter is required. The following Required = No is incorrect.
--
-- 'threshold', 'metricV2_threshold' - Contains information about the threshold for service level metrics.
newMetricV2 ::
  MetricV2
newMetricV2 =
  MetricV2'
    { metricFilters = Prelude.Nothing,
      name = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | Contains the filters to be used when returning data.
metricV2_metricFilters :: Lens.Lens' MetricV2 (Prelude.Maybe [MetricFilterV2])
metricV2_metricFilters = Lens.lens (\MetricV2' {metricFilters} -> metricFilters) (\s@MetricV2' {} a -> s {metricFilters = a} :: MetricV2) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric.
--
-- This parameter is required. The following Required = No is incorrect.
metricV2_name :: Lens.Lens' MetricV2 (Prelude.Maybe Prelude.Text)
metricV2_name = Lens.lens (\MetricV2' {name} -> name) (\s@MetricV2' {} a -> s {name = a} :: MetricV2)

-- | Contains information about the threshold for service level metrics.
metricV2_threshold :: Lens.Lens' MetricV2 (Prelude.Maybe [ThresholdV2])
metricV2_threshold = Lens.lens (\MetricV2' {threshold} -> threshold) (\s@MetricV2' {} a -> s {threshold = a} :: MetricV2) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricV2 where
  parseJSON =
    Data.withObject
      "MetricV2"
      ( \x ->
          MetricV2'
            Prelude.<$> (x Data..:? "MetricFilters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Threshold" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricV2 where
  hashWithSalt _salt MetricV2' {..} =
    _salt
      `Prelude.hashWithSalt` metricFilters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData MetricV2 where
  rnf MetricV2' {..} =
    Prelude.rnf metricFilters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON MetricV2 where
  toJSON MetricV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricFilters" Data..=) Prelude.<$> metricFilters,
            ("Name" Data..=) Prelude.<$> name,
            ("Threshold" Data..=) Prelude.<$> threshold
          ]
      )
