{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeAnomalyDetectors (..),
    mkDescribeAnomalyDetectors,

    -- ** Request lenses
    dDimensions,
    dMaxResults,
    dMetricName,
    dNamespace,
    dNextToken,

    -- * Destructuring the response
    DescribeAnomalyDetectorsResponse (..),
    mkDescribeAnomalyDetectorsResponse,

    -- ** Response lenses
    dadrfrsAnomalyDetectors,
    dadrfrsNextToken,
    dadrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAnomalyDetectors' smart constructor.
data DescribeAnomalyDetectors = DescribeAnomalyDetectors'
  { -- | Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The maximum number of results to return in one operation. The maximum value that you can specify is 100.
    --
    -- To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
    metricName :: Core.Maybe Types.MetricName,
    -- | Limits the results to only the anomaly detection models that are associated with the specified namespace.
    namespace :: Core.Maybe Types.Namespace,
    -- | Use the token returned by the previous operation to request the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAnomalyDetectors' value with any optional fields omitted.
mkDescribeAnomalyDetectors ::
  DescribeAnomalyDetectors
mkDescribeAnomalyDetectors =
  DescribeAnomalyDetectors'
    { dimensions = Core.Nothing,
      maxResults = Core.Nothing,
      metricName = Core.Nothing,
      namespace = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDimensions :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe [Types.Dimension])
dDimensions = Lens.field @"dimensions"
{-# DEPRECATED dDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The maximum number of results to return in one operation. The maximum value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMetricName :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.MetricName)
dMetricName = Lens.field @"metricName"
{-# DEPRECATED dMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNamespace :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.Namespace)
dNamespace = Lens.field @"namespace"
{-# DEPRECATED dNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | Use the token returned by the previous operation to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeAnomalyDetectors (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeAnomalyDetectors where
  type Rs DescribeAnomalyDetectors = DescribeAnomalyDetectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeAnomalyDetectors")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "Dimensions"
                            (Core.toQueryList "member" Core.<$> dimensions)
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "MetricName" Core.<$> metricName)
                Core.<> (Core.toQueryValue "Namespace" Core.<$> namespace)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAnomalyDetectorsResult"
      ( \s h x ->
          DescribeAnomalyDetectorsResponse'
            Core.<$> ( x Core..@? "AnomalyDetectors"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAnomalyDetectorsResponse' smart constructor.
data DescribeAnomalyDetectorsResponse = DescribeAnomalyDetectorsResponse'
  { -- | The list of anomaly detection models returned by the operation.
    anomalyDetectors :: Core.Maybe [Types.AnomalyDetector],
    -- | A token that you can use in a subsequent operation to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAnomalyDetectorsResponse' value with any optional fields omitted.
mkDescribeAnomalyDetectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAnomalyDetectorsResponse
mkDescribeAnomalyDetectorsResponse responseStatus =
  DescribeAnomalyDetectorsResponse'
    { anomalyDetectors =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of anomaly detection models returned by the operation.
--
-- /Note:/ Consider using 'anomalyDetectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsAnomalyDetectors :: Lens.Lens' DescribeAnomalyDetectorsResponse (Core.Maybe [Types.AnomalyDetector])
dadrfrsAnomalyDetectors = Lens.field @"anomalyDetectors"
{-# DEPRECATED dadrfrsAnomalyDetectors "Use generic-lens or generic-optics with 'anomalyDetectors' instead." #-}

-- | A token that you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsNextToken :: Lens.Lens' DescribeAnomalyDetectorsResponse (Core.Maybe Types.NextToken)
dadrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dadrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrfrsResponseStatus :: Lens.Lens' DescribeAnomalyDetectorsResponse Core.Int
dadrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dadrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
