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
-- Module      : Network.AWS.SageMaker.Types.MetricDatum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricDatum where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLMetricEnum
import Network.AWS.SageMaker.Types.MetricSetSource

-- | Information about the metric for a candidate produced by an AutoML job.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | The dataset split from which the AutoML job produced the metric.
    set :: Prelude.Maybe MetricSetSource,
    -- | The name of the metric.
    metricName :: Prelude.Maybe AutoMLMetricEnum,
    -- | The value of the metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDatum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'set', 'metricDatum_set' - The dataset split from which the AutoML job produced the metric.
--
-- 'metricName', 'metricDatum_metricName' - The name of the metric.
--
-- 'value', 'metricDatum_value' - The value of the metric.
newMetricDatum ::
  MetricDatum
newMetricDatum =
  MetricDatum'
    { set = Prelude.Nothing,
      metricName = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The dataset split from which the AutoML job produced the metric.
metricDatum_set :: Lens.Lens' MetricDatum (Prelude.Maybe MetricSetSource)
metricDatum_set = Lens.lens (\MetricDatum' {set} -> set) (\s@MetricDatum' {} a -> s {set = a} :: MetricDatum)

-- | The name of the metric.
metricDatum_metricName :: Lens.Lens' MetricDatum (Prelude.Maybe AutoMLMetricEnum)
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

-- | The value of the metric.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

instance Core.FromJSON MetricDatum where
  parseJSON =
    Core.withObject
      "MetricDatum"
      ( \x ->
          MetricDatum'
            Prelude.<$> (x Core..:? "Set")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable MetricDatum

instance Prelude.NFData MetricDatum
