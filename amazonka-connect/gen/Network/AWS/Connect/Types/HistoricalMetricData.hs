{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.Types.HistoricalMetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricData where

import Network.AWS.Connect.Types.HistoricalMetric
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the data for a historical metric.
--
-- /See:/ 'newHistoricalMetricData' smart constructor.
data HistoricalMetricData = HistoricalMetricData'
  { -- | Information about the metric.
    metric :: Prelude.Maybe HistoricalMetric,
    -- | The value of the metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HistoricalMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'historicalMetricData_metric' - Information about the metric.
--
-- 'value', 'historicalMetricData_value' - The value of the metric.
newHistoricalMetricData ::
  HistoricalMetricData
newHistoricalMetricData =
  HistoricalMetricData'
    { metric = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Information about the metric.
historicalMetricData_metric :: Lens.Lens' HistoricalMetricData (Prelude.Maybe HistoricalMetric)
historicalMetricData_metric = Lens.lens (\HistoricalMetricData' {metric} -> metric) (\s@HistoricalMetricData' {} a -> s {metric = a} :: HistoricalMetricData)

-- | The value of the metric.
historicalMetricData_value :: Lens.Lens' HistoricalMetricData (Prelude.Maybe Prelude.Double)
historicalMetricData_value = Lens.lens (\HistoricalMetricData' {value} -> value) (\s@HistoricalMetricData' {} a -> s {value = a} :: HistoricalMetricData)

instance Prelude.FromJSON HistoricalMetricData where
  parseJSON =
    Prelude.withObject
      "HistoricalMetricData"
      ( \x ->
          HistoricalMetricData'
            Prelude.<$> (x Prelude..:? "Metric")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable HistoricalMetricData

instance Prelude.NFData HistoricalMetricData
