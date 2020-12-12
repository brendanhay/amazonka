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
    adMetricName,
    adNamespace,
    adStateValue,
    adStat,
    adConfiguration,
    adDimensions,
  )
where

import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
import Network.AWS.CloudWatch.Types.Dimension
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An anomaly detection model associated with a particular CloudWatch metric and statistic. You can use the model to display a band of expected normal values when the metric is graphed.
--
-- /See:/ 'mkAnomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { metricName ::
      Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text,
    stateValue :: Lude.Maybe AnomalyDetectorStateValue,
    stat :: Lude.Maybe Lude.Text,
    configuration :: Lude.Maybe AnomalyDetectorConfiguration,
    dimensions :: Lude.Maybe [Dimension]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalyDetector' with the minimum fields required to make a request.
--
-- * 'configuration' - The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
-- * 'dimensions' - The metric dimensions associated with the anomaly detection model.
-- * 'metricName' - The name of the metric associated with the anomaly detection model.
-- * 'namespace' - The namespace of the metric associated with the anomaly detection model.
-- * 'stat' - The statistic associated with the anomaly detection model.
-- * 'stateValue' - The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
mkAnomalyDetector ::
  AnomalyDetector
mkAnomalyDetector =
  AnomalyDetector'
    { metricName = Lude.Nothing,
      namespace = Lude.Nothing,
      stateValue = Lude.Nothing,
      stat = Lude.Nothing,
      configuration = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The name of the metric associated with the anomaly detection model.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMetricName :: Lens.Lens' AnomalyDetector (Lude.Maybe Lude.Text)
adMetricName = Lens.lens (metricName :: AnomalyDetector -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: AnomalyDetector)
{-# DEPRECATED adMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric associated with the anomaly detection model.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adNamespace :: Lens.Lens' AnomalyDetector (Lude.Maybe Lude.Text)
adNamespace = Lens.lens (namespace :: AnomalyDetector -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: AnomalyDetector)
{-# DEPRECATED adNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStateValue :: Lens.Lens' AnomalyDetector (Lude.Maybe AnomalyDetectorStateValue)
adStateValue = Lens.lens (stateValue :: AnomalyDetector -> Lude.Maybe AnomalyDetectorStateValue) (\s a -> s {stateValue = a} :: AnomalyDetector)
{-# DEPRECATED adStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The statistic associated with the anomaly detection model.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStat :: Lens.Lens' AnomalyDetector (Lude.Maybe Lude.Text)
adStat = Lens.lens (stat :: AnomalyDetector -> Lude.Maybe Lude.Text) (\s a -> s {stat = a} :: AnomalyDetector)
{-# DEPRECATED adStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfiguration :: Lens.Lens' AnomalyDetector (Lude.Maybe AnomalyDetectorConfiguration)
adConfiguration = Lens.lens (configuration :: AnomalyDetector -> Lude.Maybe AnomalyDetectorConfiguration) (\s a -> s {configuration = a} :: AnomalyDetector)
{-# DEPRECATED adConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The metric dimensions associated with the anomaly detection model.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDimensions :: Lens.Lens' AnomalyDetector (Lude.Maybe [Dimension])
adDimensions = Lens.lens (dimensions :: AnomalyDetector -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: AnomalyDetector)
{-# DEPRECATED adDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromXML AnomalyDetector where
  parseXML x =
    AnomalyDetector'
      Lude.<$> (x Lude..@? "MetricName")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> (x Lude..@? "StateValue")
      Lude.<*> (x Lude..@? "Stat")
      Lude.<*> (x Lude..@? "Configuration")
      Lude.<*> ( x Lude..@? "Dimensions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
