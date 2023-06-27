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
-- Module      : Amazonka.SageMakerMetrics.Types.RawMetricData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerMetrics.Types.RawMetricData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The raw metric data to associate with the resource.
--
-- /See:/ 'newRawMetricData' smart constructor.
data RawMetricData = RawMetricData'
  { -- | The metric step (epoch).
    step :: Prelude.Maybe Prelude.Natural,
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The time that the metric was recorded.
    timestamp :: Data.POSIX,
    -- | The metric value.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RawMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'step', 'rawMetricData_step' - The metric step (epoch).
--
-- 'metricName', 'rawMetricData_metricName' - The name of the metric.
--
-- 'timestamp', 'rawMetricData_timestamp' - The time that the metric was recorded.
--
-- 'value', 'rawMetricData_value' - The metric value.
newRawMetricData ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'value'
  Prelude.Double ->
  RawMetricData
newRawMetricData pMetricName_ pTimestamp_ pValue_ =
  RawMetricData'
    { step = Prelude.Nothing,
      metricName = pMetricName_,
      timestamp = Data._Time Lens.# pTimestamp_,
      value = pValue_
    }

-- | The metric step (epoch).
rawMetricData_step :: Lens.Lens' RawMetricData (Prelude.Maybe Prelude.Natural)
rawMetricData_step = Lens.lens (\RawMetricData' {step} -> step) (\s@RawMetricData' {} a -> s {step = a} :: RawMetricData)

-- | The name of the metric.
rawMetricData_metricName :: Lens.Lens' RawMetricData Prelude.Text
rawMetricData_metricName = Lens.lens (\RawMetricData' {metricName} -> metricName) (\s@RawMetricData' {} a -> s {metricName = a} :: RawMetricData)

-- | The time that the metric was recorded.
rawMetricData_timestamp :: Lens.Lens' RawMetricData Prelude.UTCTime
rawMetricData_timestamp = Lens.lens (\RawMetricData' {timestamp} -> timestamp) (\s@RawMetricData' {} a -> s {timestamp = a} :: RawMetricData) Prelude.. Data._Time

-- | The metric value.
rawMetricData_value :: Lens.Lens' RawMetricData Prelude.Double
rawMetricData_value = Lens.lens (\RawMetricData' {value} -> value) (\s@RawMetricData' {} a -> s {value = a} :: RawMetricData)

instance Prelude.Hashable RawMetricData where
  hashWithSalt _salt RawMetricData' {..} =
    _salt
      `Prelude.hashWithSalt` step
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData RawMetricData where
  rnf RawMetricData' {..} =
    Prelude.rnf step
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON RawMetricData where
  toJSON RawMetricData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Step" Data..=) Prelude.<$> step,
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just ("Timestamp" Data..= timestamp),
            Prelude.Just ("Value" Data..= value)
          ]
      )
