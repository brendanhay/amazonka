{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutAnomalyDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an anomaly detection model for a CloudWatch metric. You can use the model to display a band of expected normal values when the metric is graphed.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Anomaly_Detection.html CloudWatch Anomaly Detection> .
module Network.AWS.CloudWatch.PutAnomalyDetector
  ( -- * Creating a request
    PutAnomalyDetector (..),
    mkPutAnomalyDetector,

    -- ** Request lenses
    padMetricName,
    padNamespace,
    padStat,
    padConfiguration,
    padDimensions,

    -- * Destructuring the response
    PutAnomalyDetectorResponse (..),
    mkPutAnomalyDetectorResponse,

    -- ** Response lenses
    padrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAnomalyDetector' smart constructor.
data PutAnomalyDetector = PutAnomalyDetector'
  { -- | The name of the metric to create the anomaly detection model for.
    metricName :: Lude.Text,
    -- | The namespace of the metric to create the anomaly detection model for.
    namespace :: Lude.Text,
    -- | The statistic to use for the metric and the anomaly detection model.
    stat :: Lude.Text,
    -- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges.
    --
    -- The configuration can also include the time zone to use for the metric.
    configuration :: Lude.Maybe AnomalyDetectorConfiguration,
    -- | The metric dimensions to create the anomaly detection model for.
    dimensions :: Lude.Maybe [Dimension]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAnomalyDetector' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric to create the anomaly detection model for.
-- * 'namespace' - The namespace of the metric to create the anomaly detection model for.
-- * 'stat' - The statistic to use for the metric and the anomaly detection model.
-- * 'configuration' - The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
-- * 'dimensions' - The metric dimensions to create the anomaly detection model for.
mkPutAnomalyDetector ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'stat'
  Lude.Text ->
  PutAnomalyDetector
mkPutAnomalyDetector pMetricName_ pNamespace_ pStat_ =
  PutAnomalyDetector'
    { metricName = pMetricName_,
      namespace = pNamespace_,
      stat = pStat_,
      configuration = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The name of the metric to create the anomaly detection model for.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padMetricName :: Lens.Lens' PutAnomalyDetector Lude.Text
padMetricName = Lens.lens (metricName :: PutAnomalyDetector -> Lude.Text) (\s a -> s {metricName = a} :: PutAnomalyDetector)
{-# DEPRECATED padMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric to create the anomaly detection model for.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padNamespace :: Lens.Lens' PutAnomalyDetector Lude.Text
padNamespace = Lens.lens (namespace :: PutAnomalyDetector -> Lude.Text) (\s a -> s {namespace = a} :: PutAnomalyDetector)
{-# DEPRECATED padNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic to use for the metric and the anomaly detection model.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padStat :: Lens.Lens' PutAnomalyDetector Lude.Text
padStat = Lens.lens (stat :: PutAnomalyDetector -> Lude.Text) (\s a -> s {stat = a} :: PutAnomalyDetector)
{-# DEPRECATED padStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padConfiguration :: Lens.Lens' PutAnomalyDetector (Lude.Maybe AnomalyDetectorConfiguration)
padConfiguration = Lens.lens (configuration :: PutAnomalyDetector -> Lude.Maybe AnomalyDetectorConfiguration) (\s a -> s {configuration = a} :: PutAnomalyDetector)
{-# DEPRECATED padConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The metric dimensions to create the anomaly detection model for.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padDimensions :: Lens.Lens' PutAnomalyDetector (Lude.Maybe [Dimension])
padDimensions = Lens.lens (dimensions :: PutAnomalyDetector -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: PutAnomalyDetector)
{-# DEPRECATED padDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.AWSRequest PutAnomalyDetector where
  type Rs PutAnomalyDetector = PutAnomalyDetectorResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "PutAnomalyDetectorResult"
      ( \s h x ->
          PutAnomalyDetectorResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAnomalyDetector where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutAnomalyDetector where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAnomalyDetector where
  toQuery PutAnomalyDetector' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutAnomalyDetector" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "Stat" Lude.=: stat,
        "Configuration" Lude.=: configuration,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions)
      ]

-- | /See:/ 'mkPutAnomalyDetectorResponse' smart constructor.
newtype PutAnomalyDetectorResponse = PutAnomalyDetectorResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAnomalyDetectorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutAnomalyDetectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAnomalyDetectorResponse
mkPutAnomalyDetectorResponse pResponseStatus_ =
  PutAnomalyDetectorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padrsResponseStatus :: Lens.Lens' PutAnomalyDetectorResponse Lude.Int
padrsResponseStatus = Lens.lens (responseStatus :: PutAnomalyDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAnomalyDetectorResponse)
{-# DEPRECATED padrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
