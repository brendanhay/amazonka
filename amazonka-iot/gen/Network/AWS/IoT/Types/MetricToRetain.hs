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
-- Module      : Network.AWS.IoT.Types.MetricToRetain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricToRetain where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.MetricDimension
import qualified Network.AWS.Lens as Lens

-- | The metric you want to retain. Dimensions are optional.
--
-- /See:/ 'newMetricToRetain' smart constructor.
data MetricToRetain = MetricToRetain'
  { -- | The dimension of a metric. This can\'t be used with custom metrics.
    metricDimension :: Core.Maybe MetricDimension,
    -- | What is measured by the behavior.
    metric :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  MetricToRetain
newMetricToRetain pMetric_ =
  MetricToRetain'
    { metricDimension = Core.Nothing,
      metric = pMetric_
    }

-- | The dimension of a metric. This can\'t be used with custom metrics.
metricToRetain_metricDimension :: Lens.Lens' MetricToRetain (Core.Maybe MetricDimension)
metricToRetain_metricDimension = Lens.lens (\MetricToRetain' {metricDimension} -> metricDimension) (\s@MetricToRetain' {} a -> s {metricDimension = a} :: MetricToRetain)

-- | What is measured by the behavior.
metricToRetain_metric :: Lens.Lens' MetricToRetain Core.Text
metricToRetain_metric = Lens.lens (\MetricToRetain' {metric} -> metric) (\s@MetricToRetain' {} a -> s {metric = a} :: MetricToRetain)

instance Core.FromJSON MetricToRetain where
  parseJSON =
    Core.withObject
      "MetricToRetain"
      ( \x ->
          MetricToRetain'
            Core.<$> (x Core..:? "metricDimension")
            Core.<*> (x Core..: "metric")
      )

instance Core.Hashable MetricToRetain

instance Core.NFData MetricToRetain

instance Core.ToJSON MetricToRetain where
  toJSON MetricToRetain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("metricDimension" Core..=)
              Core.<$> metricDimension,
            Core.Just ("metric" Core..= metric)
          ]
      )
