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
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON MetricToRetain where
  parseJSON =
    Core.withObject
      "MetricToRetain"
      ( \x ->
          MetricToRetain'
            Prelude.<$> (x Core..:? "metricDimension")
            Prelude.<*> (x Core..: "metric")
      )

instance Prelude.Hashable MetricToRetain

instance Prelude.NFData MetricToRetain

instance Core.ToJSON MetricToRetain where
  toJSON MetricToRetain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("metricDimension" Core..=)
              Prelude.<$> metricDimension,
            Prelude.Just ("metric" Core..= metric)
          ]
      )
