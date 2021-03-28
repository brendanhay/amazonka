{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified anomaly detection model from your account.
module Network.AWS.CloudWatch.DeleteAnomalyDetector
    (
    -- * Creating a request
      DeleteAnomalyDetector (..)
    , mkDeleteAnomalyDetector
    -- ** Request lenses
    , dadNamespace
    , dadMetricName
    , dadStat
    , dadDimensions

    -- * Destructuring the response
    , DeleteAnomalyDetectorResponse (..)
    , mkDeleteAnomalyDetectorResponse
    -- ** Response lenses
    , dadrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { namespace :: Types.Namespace
    -- ^ The namespace associated with the anomaly detection model to delete.
  , metricName :: Types.MetricName
    -- ^ The metric name associated with the anomaly detection model to delete.
  , stat :: Types.AnomalyDetectorMetricStat
    -- ^ The statistic associated with the anomaly detection model to delete.
  , dimensions :: Core.Maybe [Types.Dimension]
    -- ^ The metric dimensions associated with the anomaly detection model to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyDetector' value with any optional fields omitted.
mkDeleteAnomalyDetector
    :: Types.Namespace -- ^ 'namespace'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.AnomalyDetectorMetricStat -- ^ 'stat'
    -> DeleteAnomalyDetector
mkDeleteAnomalyDetector namespace metricName stat
  = DeleteAnomalyDetector'{namespace, metricName, stat,
                           dimensions = Core.Nothing}

-- | The namespace associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadNamespace :: Lens.Lens' DeleteAnomalyDetector Types.Namespace
dadNamespace = Lens.field @"namespace"
{-# INLINEABLE dadNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The metric name associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadMetricName :: Lens.Lens' DeleteAnomalyDetector Types.MetricName
dadMetricName = Lens.field @"metricName"
{-# INLINEABLE dadMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The statistic associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadStat :: Lens.Lens' DeleteAnomalyDetector Types.AnomalyDetectorMetricStat
dadStat = Lens.field @"stat"
{-# INLINEABLE dadStat #-}
{-# DEPRECATED stat "Use generic-lens or generic-optics with 'stat' instead"  #-}

-- | The metric dimensions associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadDimensions :: Lens.Lens' DeleteAnomalyDetector (Core.Maybe [Types.Dimension])
dadDimensions = Lens.field @"dimensions"
{-# INLINEABLE dadDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

instance Core.ToQuery DeleteAnomalyDetector where
        toQuery DeleteAnomalyDetector{..}
          = Core.toQueryPair "Action" ("DeleteAnomalyDetector" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "Namespace" namespace
              Core.<> Core.toQueryPair "MetricName" metricName
              Core.<> Core.toQueryPair "Stat" stat
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)

instance Core.ToHeaders DeleteAnomalyDetector where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAnomalyDetector where
        type Rs DeleteAnomalyDetector = DeleteAnomalyDetectorResponse
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
          = Response.receiveXMLWrapper "DeleteAnomalyDetectorResult"
              (\ s h x ->
                 DeleteAnomalyDetectorResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAnomalyDetectorResponse' smart constructor.
newtype DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyDetectorResponse' value with any optional fields omitted.
mkDeleteAnomalyDetectorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAnomalyDetectorResponse
mkDeleteAnomalyDetectorResponse responseStatus
  = DeleteAnomalyDetectorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrrsResponseStatus :: Lens.Lens' DeleteAnomalyDetectorResponse Core.Int
dadrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dadrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
