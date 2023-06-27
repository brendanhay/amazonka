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
-- Module      : Amazonka.Connect.Types.MetricDataV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.MetricDataV2 where

import Amazonka.Connect.Types.MetricV2
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the name, thresholds, and metric filters.
--
-- /See:/ 'newMetricDataV2' smart constructor.
data MetricDataV2 = MetricDataV2'
  { -- | The metric name, thresholds, and metric filters of the returned metric.
    metric :: Prelude.Maybe MetricV2,
    -- | The corresponding value of the metric returned in the response.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDataV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'metricDataV2_metric' - The metric name, thresholds, and metric filters of the returned metric.
--
-- 'value', 'metricDataV2_value' - The corresponding value of the metric returned in the response.
newMetricDataV2 ::
  MetricDataV2
newMetricDataV2 =
  MetricDataV2'
    { metric = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The metric name, thresholds, and metric filters of the returned metric.
metricDataV2_metric :: Lens.Lens' MetricDataV2 (Prelude.Maybe MetricV2)
metricDataV2_metric = Lens.lens (\MetricDataV2' {metric} -> metric) (\s@MetricDataV2' {} a -> s {metric = a} :: MetricDataV2)

-- | The corresponding value of the metric returned in the response.
metricDataV2_value :: Lens.Lens' MetricDataV2 (Prelude.Maybe Prelude.Double)
metricDataV2_value = Lens.lens (\MetricDataV2' {value} -> value) (\s@MetricDataV2' {} a -> s {value = a} :: MetricDataV2)

instance Data.FromJSON MetricDataV2 where
  parseJSON =
    Data.withObject
      "MetricDataV2"
      ( \x ->
          MetricDataV2'
            Prelude.<$> (x Data..:? "Metric")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable MetricDataV2 where
  hashWithSalt _salt MetricDataV2' {..} =
    _salt
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricDataV2 where
  rnf MetricDataV2' {..} =
    Prelude.rnf metric `Prelude.seq` Prelude.rnf value
