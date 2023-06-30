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
-- Module      : Amazonka.SageMaker.Types.MetricDatum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricDatum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLMetricEnum
import Amazonka.SageMaker.Types.AutoMLMetricExtendedEnum
import Amazonka.SageMaker.Types.MetricSetSource

-- | Information about the metric for a candidate produced by an AutoML job.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | The name of the metric.
    metricName :: Prelude.Maybe AutoMLMetricEnum,
    -- | The dataset split from which the AutoML job produced the metric.
    set :: Prelude.Maybe MetricSetSource,
    -- | The name of the standard metric.
    --
    -- For definitions of the standard metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-metrics Autopilot candidate metrics>
    -- .
    standardMetricName :: Prelude.Maybe AutoMLMetricExtendedEnum,
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
-- 'metricName', 'metricDatum_metricName' - The name of the metric.
--
-- 'set', 'metricDatum_set' - The dataset split from which the AutoML job produced the metric.
--
-- 'standardMetricName', 'metricDatum_standardMetricName' - The name of the standard metric.
--
-- For definitions of the standard metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-metrics Autopilot candidate metrics>
-- .
--
-- 'value', 'metricDatum_value' - The value of the metric.
newMetricDatum ::
  MetricDatum
newMetricDatum =
  MetricDatum'
    { metricName = Prelude.Nothing,
      set = Prelude.Nothing,
      standardMetricName = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the metric.
metricDatum_metricName :: Lens.Lens' MetricDatum (Prelude.Maybe AutoMLMetricEnum)
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

-- | The dataset split from which the AutoML job produced the metric.
metricDatum_set :: Lens.Lens' MetricDatum (Prelude.Maybe MetricSetSource)
metricDatum_set = Lens.lens (\MetricDatum' {set} -> set) (\s@MetricDatum' {} a -> s {set = a} :: MetricDatum)

-- | The name of the standard metric.
--
-- For definitions of the standard metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-metrics Autopilot candidate metrics>
-- .
metricDatum_standardMetricName :: Lens.Lens' MetricDatum (Prelude.Maybe AutoMLMetricExtendedEnum)
metricDatum_standardMetricName = Lens.lens (\MetricDatum' {standardMetricName} -> standardMetricName) (\s@MetricDatum' {} a -> s {standardMetricName = a} :: MetricDatum)

-- | The value of the metric.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

instance Data.FromJSON MetricDatum where
  parseJSON =
    Data.withObject
      "MetricDatum"
      ( \x ->
          MetricDatum'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Set")
            Prelude.<*> (x Data..:? "StandardMetricName")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable MetricDatum where
  hashWithSalt _salt MetricDatum' {..} =
    _salt
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` set
      `Prelude.hashWithSalt` standardMetricName
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricDatum where
  rnf MetricDatum' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf set
      `Prelude.seq` Prelude.rnf standardMetricName
      `Prelude.seq` Prelude.rnf value
