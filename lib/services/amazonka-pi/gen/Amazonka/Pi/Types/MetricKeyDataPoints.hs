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
-- Module      : Amazonka.Pi.Types.MetricKeyDataPoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.MetricKeyDataPoints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pi.Types.DataPoint
import Amazonka.Pi.Types.ResponseResourceMetricKey
import qualified Amazonka.Prelude as Prelude

-- | A time-ordered series of data points, corresponding to a dimension of a
-- Performance Insights metric.
--
-- /See:/ 'newMetricKeyDataPoints' smart constructor.
data MetricKeyDataPoints = MetricKeyDataPoints'
  { -- | An array of timestamp-value pairs, representing measurements over a
    -- period of time.
    dataPoints :: Prelude.Maybe [DataPoint],
    -- | The dimensions to which the data points apply.
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
-- 'key', 'metricKeyDataPoints_key' - The dimensions to which the data points apply.
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

-- | The dimensions to which the data points apply.
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

instance Prelude.Hashable MetricKeyDataPoints where
  hashWithSalt _salt MetricKeyDataPoints' {..} =
    _salt `Prelude.hashWithSalt` dataPoints
      `Prelude.hashWithSalt` key

instance Prelude.NFData MetricKeyDataPoints where
  rnf MetricKeyDataPoints' {..} =
    Prelude.rnf dataPoints
      `Prelude.seq` Prelude.rnf key
