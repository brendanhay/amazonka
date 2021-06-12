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
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetector where

import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
import Network.AWS.CloudWatch.Types.Dimension
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An anomaly detection model associated with a particular CloudWatch
-- metric and statistic. You can use the model to display a band of
-- expected normal values when the metric is graphed.
--
-- /See:/ 'newAnomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { -- | The name of the metric associated with the anomaly detection model.
    metricName :: Core.Maybe Core.Text,
    -- | The configuration specifies details about how the anomaly detection
    -- model is to be trained, including time ranges to exclude from use for
    -- training the model, and the time zone to use for the metric.
    configuration :: Core.Maybe AnomalyDetectorConfiguration,
    -- | The current status of the anomaly detector\'s training. The possible
    -- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
    stateValue :: Core.Maybe AnomalyDetectorStateValue,
    -- | The metric dimensions associated with the anomaly detection model.
    dimensions :: Core.Maybe [Dimension],
    -- | The namespace of the metric associated with the anomaly detection model.
    namespace :: Core.Maybe Core.Text,
    -- | The statistic associated with the anomaly detection model.
    stat :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'anomalyDetector_metricName' - The name of the metric associated with the anomaly detection model.
--
-- 'configuration', 'anomalyDetector_configuration' - The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude from use for
-- training the model, and the time zone to use for the metric.
--
-- 'stateValue', 'anomalyDetector_stateValue' - The current status of the anomaly detector\'s training. The possible
-- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
--
-- 'dimensions', 'anomalyDetector_dimensions' - The metric dimensions associated with the anomaly detection model.
--
-- 'namespace', 'anomalyDetector_namespace' - The namespace of the metric associated with the anomaly detection model.
--
-- 'stat', 'anomalyDetector_stat' - The statistic associated with the anomaly detection model.
newAnomalyDetector ::
  AnomalyDetector
newAnomalyDetector =
  AnomalyDetector'
    { metricName = Core.Nothing,
      configuration = Core.Nothing,
      stateValue = Core.Nothing,
      dimensions = Core.Nothing,
      namespace = Core.Nothing,
      stat = Core.Nothing
    }

-- | The name of the metric associated with the anomaly detection model.
anomalyDetector_metricName :: Lens.Lens' AnomalyDetector (Core.Maybe Core.Text)
anomalyDetector_metricName = Lens.lens (\AnomalyDetector' {metricName} -> metricName) (\s@AnomalyDetector' {} a -> s {metricName = a} :: AnomalyDetector)

-- | The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude from use for
-- training the model, and the time zone to use for the metric.
anomalyDetector_configuration :: Lens.Lens' AnomalyDetector (Core.Maybe AnomalyDetectorConfiguration)
anomalyDetector_configuration = Lens.lens (\AnomalyDetector' {configuration} -> configuration) (\s@AnomalyDetector' {} a -> s {configuration = a} :: AnomalyDetector)

-- | The current status of the anomaly detector\'s training. The possible
-- values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
anomalyDetector_stateValue :: Lens.Lens' AnomalyDetector (Core.Maybe AnomalyDetectorStateValue)
anomalyDetector_stateValue = Lens.lens (\AnomalyDetector' {stateValue} -> stateValue) (\s@AnomalyDetector' {} a -> s {stateValue = a} :: AnomalyDetector)

-- | The metric dimensions associated with the anomaly detection model.
anomalyDetector_dimensions :: Lens.Lens' AnomalyDetector (Core.Maybe [Dimension])
anomalyDetector_dimensions = Lens.lens (\AnomalyDetector' {dimensions} -> dimensions) (\s@AnomalyDetector' {} a -> s {dimensions = a} :: AnomalyDetector) Core.. Lens.mapping Lens._Coerce

-- | The namespace of the metric associated with the anomaly detection model.
anomalyDetector_namespace :: Lens.Lens' AnomalyDetector (Core.Maybe Core.Text)
anomalyDetector_namespace = Lens.lens (\AnomalyDetector' {namespace} -> namespace) (\s@AnomalyDetector' {} a -> s {namespace = a} :: AnomalyDetector)

-- | The statistic associated with the anomaly detection model.
anomalyDetector_stat :: Lens.Lens' AnomalyDetector (Core.Maybe Core.Text)
anomalyDetector_stat = Lens.lens (\AnomalyDetector' {stat} -> stat) (\s@AnomalyDetector' {} a -> s {stat = a} :: AnomalyDetector)

instance Core.FromXML AnomalyDetector where
  parseXML x =
    AnomalyDetector'
      Core.<$> (x Core..@? "MetricName")
      Core.<*> (x Core..@? "Configuration")
      Core.<*> (x Core..@? "StateValue")
      Core.<*> ( x Core..@? "Dimensions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Namespace")
      Core.<*> (x Core..@? "Stat")

instance Core.Hashable AnomalyDetector

instance Core.NFData AnomalyDetector
