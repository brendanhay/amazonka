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
-- Module      : Amazonka.SageMaker.Types.MetricData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name, value, and date and time of a metric that was emitted to
-- Amazon CloudWatch.
--
-- /See:/ 'newMetricData' smart constructor.
data MetricData = MetricData'
  { -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the algorithm emitted the metric.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The value of the metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'metricData_metricName' - The name of the metric.
--
-- 'timestamp', 'metricData_timestamp' - The date and time that the algorithm emitted the metric.
--
-- 'value', 'metricData_value' - The value of the metric.
newMetricData ::
  MetricData
newMetricData =
  MetricData'
    { metricName = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the metric.
metricData_metricName :: Lens.Lens' MetricData (Prelude.Maybe Prelude.Text)
metricData_metricName = Lens.lens (\MetricData' {metricName} -> metricName) (\s@MetricData' {} a -> s {metricName = a} :: MetricData)

-- | The date and time that the algorithm emitted the metric.
metricData_timestamp :: Lens.Lens' MetricData (Prelude.Maybe Prelude.UTCTime)
metricData_timestamp = Lens.lens (\MetricData' {timestamp} -> timestamp) (\s@MetricData' {} a -> s {timestamp = a} :: MetricData) Prelude.. Lens.mapping Data._Time

-- | The value of the metric.
metricData_value :: Lens.Lens' MetricData (Prelude.Maybe Prelude.Double)
metricData_value = Lens.lens (\MetricData' {value} -> value) (\s@MetricData' {} a -> s {value = a} :: MetricData)

instance Data.FromJSON MetricData where
  parseJSON =
    Data.withObject
      "MetricData"
      ( \x ->
          MetricData'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Timestamp")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable MetricData where
  hashWithSalt _salt MetricData' {..} =
    _salt `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricData where
  rnf MetricData' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
