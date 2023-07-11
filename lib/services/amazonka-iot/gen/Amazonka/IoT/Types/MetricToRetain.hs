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
-- Module      : Amazonka.IoT.Types.MetricToRetain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MetricToRetain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.MetricDimension
import qualified Amazonka.Prelude as Prelude

-- | The metric you want to retain. Dimensions are optional.
--
-- /See:/ 'newMetricToRetain' smart constructor.
data MetricToRetain = MetricToRetain'
  { -- | The dimension of a metric. This can\'t be used with custom metrics.
    metricDimension :: Prelude.Maybe MetricDimension,
    -- | What is measured by the behavior.
    metric :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricToRetain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDimension', 'metricToRetain_metricDimension' - The dimension of a metric. This can\'t be used with custom metrics.
--
-- 'metric', 'metricToRetain_metric' - What is measured by the behavior.
newMetricToRetain ::
  -- | 'metric'
  Prelude.Text ->
  MetricToRetain
newMetricToRetain pMetric_ =
  MetricToRetain'
    { metricDimension = Prelude.Nothing,
      metric = pMetric_
    }

-- | The dimension of a metric. This can\'t be used with custom metrics.
metricToRetain_metricDimension :: Lens.Lens' MetricToRetain (Prelude.Maybe MetricDimension)
metricToRetain_metricDimension = Lens.lens (\MetricToRetain' {metricDimension} -> metricDimension) (\s@MetricToRetain' {} a -> s {metricDimension = a} :: MetricToRetain)

-- | What is measured by the behavior.
metricToRetain_metric :: Lens.Lens' MetricToRetain Prelude.Text
metricToRetain_metric = Lens.lens (\MetricToRetain' {metric} -> metric) (\s@MetricToRetain' {} a -> s {metric = a} :: MetricToRetain)

instance Data.FromJSON MetricToRetain where
  parseJSON =
    Data.withObject
      "MetricToRetain"
      ( \x ->
          MetricToRetain'
            Prelude.<$> (x Data..:? "metricDimension")
            Prelude.<*> (x Data..: "metric")
      )

instance Prelude.Hashable MetricToRetain where
  hashWithSalt _salt MetricToRetain' {..} =
    _salt
      `Prelude.hashWithSalt` metricDimension
      `Prelude.hashWithSalt` metric

instance Prelude.NFData MetricToRetain where
  rnf MetricToRetain' {..} =
    Prelude.rnf metricDimension
      `Prelude.seq` Prelude.rnf metric

instance Data.ToJSON MetricToRetain where
  toJSON MetricToRetain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metricDimension" Data..=)
              Prelude.<$> metricDimension,
            Prelude.Just ("metric" Data..= metric)
          ]
      )
