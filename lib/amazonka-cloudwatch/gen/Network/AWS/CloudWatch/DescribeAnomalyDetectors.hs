{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAnomalyDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the anomaly detection models that you have created in your account. You can list all models in your account or filter the results to only the models that are related to a certain namespace, metric name, or metric dimension.
module Network.AWS.CloudWatch.DescribeAnomalyDetectors
    (
    -- * Creating a request
      DescribeAnomalyDetectors (..)
    , mkDescribeAnomalyDetectors
    -- ** Request lenses
    , dDimensions
    , dMaxResults
    , dMetricName
    , dNamespace
    , dNextToken

    -- * Destructuring the response
    , DescribeAnomalyDetectorsResponse (..)
    , mkDescribeAnomalyDetectorsResponse
    -- ** Response lenses
    , dadrfrsAnomalyDetectors
    , dadrfrsNextToken
    , dadrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAnomalyDetectors' smart constructor.
data DescribeAnomalyDetectors = DescribeAnomalyDetectors'
  { dimensions :: Core.Maybe [Types.Dimension]
    -- ^ Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in one operation. The maximum value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned @NextToken@ value. 
  , metricName :: Core.Maybe Types.MetricName
    -- ^ Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
  , namespace :: Core.Maybe Types.Namespace
    -- ^ Limits the results to only the anomaly detection models that are associated with the specified namespace.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Use the token returned by the previous operation to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAnomalyDetectors' value with any optional fields omitted.
mkDescribeAnomalyDetectors
    :: DescribeAnomalyDetectors
mkDescribeAnomalyDetectors
  = DescribeAnomalyDetectors'{dimensions = Core.Nothing,
                              maxResults = Core.Nothing, metricName = Core.Nothing,
                              namespace = Core.Nothing, nextToken = Core.Nothing}

-- | Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDimensions :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe [Types.Dimension])
dDimensions = Lens.field @"dimensions"
{-# INLINEABLE dDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | The maximum number of results to return in one operation. The maximum value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned @NextToken@ value. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMetricName :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.MetricName)
dMetricName = Lens.field @"metricName"
{-# INLINEABLE dMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNamespace :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.Namespace)
dNamespace = Lens.field @"namespace"
{-# INLINEABLE dNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | Use the token returned by the previous operation to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAnomalyDetectors where
        toQuery DescribeAnomalyDetectors{..}
          = Core.toQueryPair "Action"
              ("DescribeAnomalyDetectors" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetricName") metricName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Namespace") namespace
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeAnomalyDetectors where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAnomalyDetectors where
        type Rs DescribeAnomalyDetectors = DescribeAnomalyDetectorsResponse
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
          = Response.receiveXMLWrapper "DescribeAnomalyDetectorsResult"
              (\ s h x ->
                 DescribeAnomalyDetectorsResponse' Core.<$>
                   (x Core..@? "AnomalyDetectors" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAnomalyDetectorsResponse' smart constructor.
data DescribeAnomalyDetectorsResponse = DescribeAnomalyDetectorsResponse'
  { anomalyDetectors :: Core.Maybe [Types.AnomalyDetector]
    -- ^ The list of anomaly detection models returned by the operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that you can use in a subsequent operation to retrieve the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAnomalyDetectorsResponse' value with any optional fields omitted.
mkDescribeAnomalyDetectorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAnomalyDetectorsResponse
mkDescribeAnomalyDetectorsResponse responseStatus
  = DescribeAnomalyDetectorsResponse'{anomalyDetectors =
                                        Core.Nothing,
                                      nextToken = Core.Nothing, responseStatus}

-- | The list of anomaly detection models returned by the operation.
--
-- /Note:/ Consider using 'anomalyDetectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsAnomalyDetectors :: Lens.Lens' DescribeAnomalyDetectorsResponse (Core.Maybe [Types.AnomalyDetector])
dadrfrsAnomalyDetectors = Lens.field @"anomalyDetectors"
{-# INLINEABLE dadrfrsAnomalyDetectors #-}
{-# DEPRECATED anomalyDetectors "Use generic-lens or generic-optics with 'anomalyDetectors' instead"  #-}

-- | A token that you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsNextToken :: Lens.Lens' DescribeAnomalyDetectorsResponse (Core.Maybe Types.NextToken)
dadrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dadrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsResponseStatus :: Lens.Lens' DescribeAnomalyDetectorsResponse Core.Int
dadrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dadrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
