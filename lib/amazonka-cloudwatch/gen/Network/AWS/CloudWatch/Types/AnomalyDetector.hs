{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetector
  ( AnomalyDetector (..),

    -- * Smart constructor
    mkAnomalyDetector,

    -- * Lenses
    adConfiguration,
    adDimensions,
    adMetricName,
    adNamespace,
    adStat,
    adStateValue,
  )
where

import qualified Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration as Types
import qualified Network.AWS.CloudWatch.Types.AnomalyDetectorMetricStat as Types
import qualified Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue as Types
import qualified Network.AWS.CloudWatch.Types.Dimension as Types
import qualified Network.AWS.CloudWatch.Types.MetricName as Types
import qualified Network.AWS.CloudWatch.Types.Namespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An anomaly detection model associated with a particular CloudWatch metric and statistic. You can use the model to display a band of expected normal values when the metric is graphed.
--
-- /See:/ 'mkAnomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { -- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
    configuration :: Core.Maybe Types.AnomalyDetectorConfiguration,
    -- | The metric dimensions associated with the anomaly detection model.
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The name of the metric associated with the anomaly detection model.
    metricName :: Core.Maybe Types.MetricName,
    -- | The namespace of the metric associated with the anomaly detection model.
    namespace :: Core.Maybe Types.Namespace,
    -- | The statistic associated with the anomaly detection model.
    stat :: Core.Maybe Types.AnomalyDetectorMetricStat,
    -- | The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
    stateValue :: Core.Maybe Types.AnomalyDetectorStateValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AnomalyDetector' value with any optional fields omitted.
mkAnomalyDetector ::
  AnomalyDetector
mkAnomalyDetector =
  AnomalyDetector'
    { configuration = Core.Nothing,
      dimensions = Core.Nothing,
      metricName = Core.Nothing,
      namespace = Core.Nothing,
      stat = Core.Nothing,
      stateValue = Core.Nothing
    }

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfiguration :: Lens.Lens' AnomalyDetector (Core.Maybe Types.AnomalyDetectorConfiguration)
adConfiguration = Lens.field @"configuration"
{-# DEPRECATED adConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The metric dimensions associated with the anomaly detection model.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDimensions :: Lens.Lens' AnomalyDetector (Core.Maybe [Types.Dimension])
adDimensions = Lens.field @"dimensions"
{-# DEPRECATED adDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The name of the metric associated with the anomaly detection model.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMetricName :: Lens.Lens' AnomalyDetector (Core.Maybe Types.MetricName)
adMetricName = Lens.field @"metricName"
{-# DEPRECATED adMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric associated with the anomaly detection model.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adNamespace :: Lens.Lens' AnomalyDetector (Core.Maybe Types.Namespace)
adNamespace = Lens.field @"namespace"
{-# DEPRECATED adNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic associated with the anomaly detection model.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStat :: Lens.Lens' AnomalyDetector (Core.Maybe Types.AnomalyDetectorMetricStat)
adStat = Lens.field @"stat"
{-# DEPRECATED adStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStateValue :: Lens.Lens' AnomalyDetector (Core.Maybe Types.AnomalyDetectorStateValue)
adStateValue = Lens.field @"stateValue"
{-# DEPRECATED adStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

instance Core.FromXML AnomalyDetector where
  parseXML x =
    AnomalyDetector'
      Core.<$> (x Core..@? "Configuration")
      Core.<*> (x Core..@? "Dimensions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "MetricName")
      Core.<*> (x Core..@? "Namespace")
      Core.<*> (x Core..@? "Stat")
      Core.<*> (x Core..@? "StateValue")
