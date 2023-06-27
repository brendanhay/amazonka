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
-- Module      : Amazonka.CloudWatch.Types.AnomalyDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.AnomalyDetector where

import Amazonka.CloudWatch.Types.AnomalyDetectorConfiguration
import Amazonka.CloudWatch.Types.AnomalyDetectorStateValue
import Amazonka.CloudWatch.Types.Dimension
import Amazonka.CloudWatch.Types.MetricMathAnomalyDetector
import Amazonka.CloudWatch.Types.SingleMetricAnomalyDetector
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An anomaly detection model associated with a particular CloudWatch
-- metric, statistic, or metric math expression. You can use the model to
-- display a band of expected, normal values when the metric is graphed.
--
-- /See:/ 'newAnomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { -- | The configuration specifies details about how the anomaly detection
    -- model is to be trained, including time ranges to exclude from use for
    -- training the model, and the time zone to use for the metric.
    configuration :: Prelude.Maybe AnomalyDetectorConfiguration,
    -- | The metric dimensions associated with the anomaly detection model.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The CloudWatch metric math expression for this anomaly detector.
    metricMathAnomalyDetector :: Prelude.Maybe MetricMathAnomalyDetector,
    -- | The name of the metric associated with the anomaly detection model.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric associated with the anomaly detection model.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch metric and statistic for this anomaly detector.
    singleMetricAnomalyDetector :: Prelude.Maybe SingleMetricAnomalyDetector,
    -- | The statistic associated with the anomaly detection model.
    stat :: Prelude.Maybe Prelude.Text,
    -- | The current status of the anomaly detector\'s training. The possible
    -- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
    stateValue :: Prelude.Maybe AnomalyDetectorStateValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'anomalyDetector_configuration' - The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude from use for
-- training the model, and the time zone to use for the metric.
--
-- 'dimensions', 'anomalyDetector_dimensions' - The metric dimensions associated with the anomaly detection model.
--
-- 'metricMathAnomalyDetector', 'anomalyDetector_metricMathAnomalyDetector' - The CloudWatch metric math expression for this anomaly detector.
--
-- 'metricName', 'anomalyDetector_metricName' - The name of the metric associated with the anomaly detection model.
--
-- 'namespace', 'anomalyDetector_namespace' - The namespace of the metric associated with the anomaly detection model.
--
-- 'singleMetricAnomalyDetector', 'anomalyDetector_singleMetricAnomalyDetector' - The CloudWatch metric and statistic for this anomaly detector.
--
-- 'stat', 'anomalyDetector_stat' - The statistic associated with the anomaly detection model.
--
-- 'stateValue', 'anomalyDetector_stateValue' - The current status of the anomaly detector\'s training. The possible
-- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
newAnomalyDetector ::
  AnomalyDetector
newAnomalyDetector =
  AnomalyDetector'
    { configuration = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      metricMathAnomalyDetector = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      singleMetricAnomalyDetector = Prelude.Nothing,
      stat = Prelude.Nothing,
      stateValue = Prelude.Nothing
    }

-- | The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude from use for
-- training the model, and the time zone to use for the metric.
anomalyDetector_configuration :: Lens.Lens' AnomalyDetector (Prelude.Maybe AnomalyDetectorConfiguration)
anomalyDetector_configuration = Lens.lens (\AnomalyDetector' {configuration} -> configuration) (\s@AnomalyDetector' {} a -> s {configuration = a} :: AnomalyDetector)

-- | The metric dimensions associated with the anomaly detection model.
anomalyDetector_dimensions :: Lens.Lens' AnomalyDetector (Prelude.Maybe [Dimension])
anomalyDetector_dimensions = Lens.lens (\AnomalyDetector' {dimensions} -> dimensions) (\s@AnomalyDetector' {} a -> s {dimensions = a} :: AnomalyDetector) Prelude.. Lens.mapping Lens.coerced

-- | The CloudWatch metric math expression for this anomaly detector.
anomalyDetector_metricMathAnomalyDetector :: Lens.Lens' AnomalyDetector (Prelude.Maybe MetricMathAnomalyDetector)
anomalyDetector_metricMathAnomalyDetector = Lens.lens (\AnomalyDetector' {metricMathAnomalyDetector} -> metricMathAnomalyDetector) (\s@AnomalyDetector' {} a -> s {metricMathAnomalyDetector = a} :: AnomalyDetector)

-- | The name of the metric associated with the anomaly detection model.
anomalyDetector_metricName :: Lens.Lens' AnomalyDetector (Prelude.Maybe Prelude.Text)
anomalyDetector_metricName = Lens.lens (\AnomalyDetector' {metricName} -> metricName) (\s@AnomalyDetector' {} a -> s {metricName = a} :: AnomalyDetector)

-- | The namespace of the metric associated with the anomaly detection model.
anomalyDetector_namespace :: Lens.Lens' AnomalyDetector (Prelude.Maybe Prelude.Text)
anomalyDetector_namespace = Lens.lens (\AnomalyDetector' {namespace} -> namespace) (\s@AnomalyDetector' {} a -> s {namespace = a} :: AnomalyDetector)

-- | The CloudWatch metric and statistic for this anomaly detector.
anomalyDetector_singleMetricAnomalyDetector :: Lens.Lens' AnomalyDetector (Prelude.Maybe SingleMetricAnomalyDetector)
anomalyDetector_singleMetricAnomalyDetector = Lens.lens (\AnomalyDetector' {singleMetricAnomalyDetector} -> singleMetricAnomalyDetector) (\s@AnomalyDetector' {} a -> s {singleMetricAnomalyDetector = a} :: AnomalyDetector)

-- | The statistic associated with the anomaly detection model.
anomalyDetector_stat :: Lens.Lens' AnomalyDetector (Prelude.Maybe Prelude.Text)
anomalyDetector_stat = Lens.lens (\AnomalyDetector' {stat} -> stat) (\s@AnomalyDetector' {} a -> s {stat = a} :: AnomalyDetector)

-- | The current status of the anomaly detector\'s training. The possible
-- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
anomalyDetector_stateValue :: Lens.Lens' AnomalyDetector (Prelude.Maybe AnomalyDetectorStateValue)
anomalyDetector_stateValue = Lens.lens (\AnomalyDetector' {stateValue} -> stateValue) (\s@AnomalyDetector' {} a -> s {stateValue = a} :: AnomalyDetector)

instance Data.FromXML AnomalyDetector where
  parseXML x =
    AnomalyDetector'
      Prelude.<$> (x Data..@? "Configuration")
      Prelude.<*> ( x
                      Data..@? "Dimensions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "MetricMathAnomalyDetector")
      Prelude.<*> (x Data..@? "MetricName")
      Prelude.<*> (x Data..@? "Namespace")
      Prelude.<*> (x Data..@? "SingleMetricAnomalyDetector")
      Prelude.<*> (x Data..@? "Stat")
      Prelude.<*> (x Data..@? "StateValue")

instance Prelude.Hashable AnomalyDetector where
  hashWithSalt _salt AnomalyDetector' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricMathAnomalyDetector
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` singleMetricAnomalyDetector
      `Prelude.hashWithSalt` stat
      `Prelude.hashWithSalt` stateValue

instance Prelude.NFData AnomalyDetector where
  rnf AnomalyDetector' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricMathAnomalyDetector
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf singleMetricAnomalyDetector
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf stateValue
