{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutAnomalyDetector (..)
    , mkPutAnomalyDetector
    -- ** Request lenses
    , padNamespace
    , padMetricName
    , padStat
    , padConfiguration
    , padDimensions

    -- * Destructuring the response
    , PutAnomalyDetectorResponse (..)
    , mkPutAnomalyDetectorResponse
    -- ** Response lenses
    , padrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAnomalyDetector' smart constructor.
data PutAnomalyDetector = PutAnomalyDetector'
  { namespace :: Types.Namespace
    -- ^ The namespace of the metric to create the anomaly detection model for.
  , metricName :: Types.MetricName
    -- ^ The name of the metric to create the anomaly detection model for.
  , stat :: Types.AnomalyDetectorMetricStat
    -- ^ The statistic to use for the metric and the anomaly detection model.
  , configuration :: Core.Maybe Types.AnomalyDetectorConfiguration
    -- ^ The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
  , dimensions :: Core.Maybe [Types.Dimension]
    -- ^ The metric dimensions to create the anomaly detection model for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutAnomalyDetector' value with any optional fields omitted.
mkPutAnomalyDetector
    :: Types.Namespace -- ^ 'namespace'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.AnomalyDetectorMetricStat -- ^ 'stat'
    -> PutAnomalyDetector
mkPutAnomalyDetector namespace metricName stat
  = PutAnomalyDetector'{namespace, metricName, stat,
                        configuration = Core.Nothing, dimensions = Core.Nothing}

-- | The namespace of the metric to create the anomaly detection model for.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padNamespace :: Lens.Lens' PutAnomalyDetector Types.Namespace
padNamespace = Lens.field @"namespace"
{-# INLINEABLE padNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The name of the metric to create the anomaly detection model for.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padMetricName :: Lens.Lens' PutAnomalyDetector Types.MetricName
padMetricName = Lens.field @"metricName"
{-# INLINEABLE padMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The statistic to use for the metric and the anomaly detection model.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padStat :: Lens.Lens' PutAnomalyDetector Types.AnomalyDetectorMetricStat
padStat = Lens.field @"stat"
{-# INLINEABLE padStat #-}
{-# DEPRECATED stat "Use generic-lens or generic-optics with 'stat' instead"  #-}

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padConfiguration :: Lens.Lens' PutAnomalyDetector (Core.Maybe Types.AnomalyDetectorConfiguration)
padConfiguration = Lens.field @"configuration"
{-# INLINEABLE padConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The metric dimensions to create the anomaly detection model for.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padDimensions :: Lens.Lens' PutAnomalyDetector (Core.Maybe [Types.Dimension])
padDimensions = Lens.field @"dimensions"
{-# INLINEABLE padDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

instance Core.ToQuery PutAnomalyDetector where
        toQuery PutAnomalyDetector{..}
          = Core.toQueryPair "Action" ("PutAnomalyDetector" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "Namespace" namespace
              Core.<> Core.toQueryPair "MetricName" metricName
              Core.<> Core.toQueryPair "Stat" stat
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Configuration")
                configuration
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)

instance Core.ToHeaders PutAnomalyDetector where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutAnomalyDetector where
        type Rs PutAnomalyDetector = PutAnomalyDetectorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "PutAnomalyDetectorResult"
              (\ s h x ->
                 PutAnomalyDetectorResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAnomalyDetectorResponse' smart constructor.
newtype PutAnomalyDetectorResponse = PutAnomalyDetectorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAnomalyDetectorResponse' value with any optional fields omitted.
mkPutAnomalyDetectorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAnomalyDetectorResponse
mkPutAnomalyDetectorResponse responseStatus
  = PutAnomalyDetectorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padrrsResponseStatus :: Lens.Lens' PutAnomalyDetectorResponse Core.Int
padrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE padrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
