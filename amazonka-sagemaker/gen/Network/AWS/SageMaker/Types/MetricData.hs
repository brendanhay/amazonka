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
-- Module      : Network.AWS.SageMaker.Types.MetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The name, value, and date and time of a metric that was emitted to
-- Amazon CloudWatch.
--
-- /See:/ 'newMetricData' smart constructor.
data MetricData = MetricData'
  { -- | The name of the metric.
    metricName :: Core.Maybe Core.Text,
    -- | The date and time that the algorithm emitted the metric.
    timestamp :: Core.Maybe Core.POSIX,
    -- | The value of the metric.
    value :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { metricName = Core.Nothing,
      timestamp = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the metric.
metricData_metricName :: Lens.Lens' MetricData (Core.Maybe Core.Text)
metricData_metricName = Lens.lens (\MetricData' {metricName} -> metricName) (\s@MetricData' {} a -> s {metricName = a} :: MetricData)

-- | The date and time that the algorithm emitted the metric.
metricData_timestamp :: Lens.Lens' MetricData (Core.Maybe Core.UTCTime)
metricData_timestamp = Lens.lens (\MetricData' {timestamp} -> timestamp) (\s@MetricData' {} a -> s {timestamp = a} :: MetricData) Core.. Lens.mapping Core._Time

-- | The value of the metric.
metricData_value :: Lens.Lens' MetricData (Core.Maybe Core.Double)
metricData_value = Lens.lens (\MetricData' {value} -> value) (\s@MetricData' {} a -> s {value = a} :: MetricData)

instance Core.FromJSON MetricData where
  parseJSON =
    Core.withObject
      "MetricData"
      ( \x ->
          MetricData'
            Core.<$> (x Core..:? "MetricName")
            Core.<*> (x Core..:? "Timestamp")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable MetricData

instance Core.NFData MetricData
