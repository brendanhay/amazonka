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
-- Module      : Network.AWS.Pi.Types.MetricKeyDataPoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pi.Types.MetricKeyDataPoints where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pi.Types.DataPoint
import Network.AWS.Pi.Types.ResponseResourceMetricKey
import qualified Network.AWS.Prelude as Prelude

-- | A time-ordered series of data points, corresponding to a dimension of a
-- Performance Insights metric.
--
-- /See:/ 'newMetricKeyDataPoints' smart constructor.
data MetricKeyDataPoints = MetricKeyDataPoints'
  { -- | An array of timestamp-value pairs, representing measurements over a
    -- period of time.
    dataPoints :: Prelude.Maybe [DataPoint],
    -- | The dimension(s) to which the data points apply.
    key :: Prelude.Maybe ResponseResourceMetricKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricKeyDataPoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPoints', 'metricKeyDataPoints_dataPoints' - An array of timestamp-value pairs, representing measurements over a
-- period of time.
--
-- 'key', 'metricKeyDataPoints_key' - The dimension(s) to which the data points apply.
newMetricKeyDataPoints ::
  MetricKeyDataPoints
newMetricKeyDataPoints =
  MetricKeyDataPoints'
    { dataPoints = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | An array of timestamp-value pairs, representing measurements over a
-- period of time.
metricKeyDataPoints_dataPoints :: Lens.Lens' MetricKeyDataPoints (Prelude.Maybe [DataPoint])
metricKeyDataPoints_dataPoints = Lens.lens (\MetricKeyDataPoints' {dataPoints} -> dataPoints) (\s@MetricKeyDataPoints' {} a -> s {dataPoints = a} :: MetricKeyDataPoints) Prelude.. Lens.mapping Lens.coerced

-- | The dimension(s) to which the data points apply.
metricKeyDataPoints_key :: Lens.Lens' MetricKeyDataPoints (Prelude.Maybe ResponseResourceMetricKey)
metricKeyDataPoints_key = Lens.lens (\MetricKeyDataPoints' {key} -> key) (\s@MetricKeyDataPoints' {} a -> s {key = a} :: MetricKeyDataPoints)

instance Core.FromJSON MetricKeyDataPoints where
  parseJSON =
    Core.withObject
      "MetricKeyDataPoints"
      ( \x ->
          MetricKeyDataPoints'
            Prelude.<$> (x Core..:? "DataPoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable MetricKeyDataPoints

instance Prelude.NFData MetricKeyDataPoints
