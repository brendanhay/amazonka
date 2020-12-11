{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dMetricName,
    dNamespace,
    dNextToken,
    dDimensions,
    dMaxResults,

    -- * Destructuring the response
    DescribeAnomalyDetectorsResponse (..),
    mkDescribeAnomalyDetectorsResponse,

    -- ** Response lenses
    dadrsAnomalyDetectors,
    dadrsNextToken,
    dadrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAnomalyDetectors' smart constructor.
data DescribeAnomalyDetectors = DescribeAnomalyDetectors'
  { metricName ::
      Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    dimensions :: Lude.Maybe [Dimension],
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAnomalyDetectors' with the minimum fields required to make a request.
--
-- * 'dimensions' - Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
-- * 'maxResults' - The maximum number of results to return in one operation. The maximum value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'metricName' - Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
-- * 'namespace' - Limits the results to only the anomaly detection models that are associated with the specified namespace.
-- * 'nextToken' - Use the token returned by the previous operation to request the next page of results.
mkDescribeAnomalyDetectors ::
  DescribeAnomalyDetectors
mkDescribeAnomalyDetectors =
  DescribeAnomalyDetectors'
    { metricName = Lude.Nothing,
      namespace = Lude.Nothing,
      nextToken = Lude.Nothing,
      dimensions = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMetricName :: Lens.Lens' DescribeAnomalyDetectors (Lude.Maybe Lude.Text)
dMetricName = Lens.lens (metricName :: DescribeAnomalyDetectors -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: DescribeAnomalyDetectors)
{-# DEPRECATED dMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNamespace :: Lens.Lens' DescribeAnomalyDetectors (Lude.Maybe Lude.Text)
dNamespace = Lens.lens (namespace :: DescribeAnomalyDetectors -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: DescribeAnomalyDetectors)
{-# DEPRECATED dNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | Use the token returned by the previous operation to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeAnomalyDetectors (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeAnomalyDetectors -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAnomalyDetectors)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDimensions :: Lens.Lens' DescribeAnomalyDetectors (Lude.Maybe [Dimension])
dDimensions = Lens.lens (dimensions :: DescribeAnomalyDetectors -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: DescribeAnomalyDetectors)
{-# DEPRECATED dDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The maximum number of results to return in one operation. The maximum value that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeAnomalyDetectors (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeAnomalyDetectors -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAnomalyDetectors)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeAnomalyDetectors where
  type Rs DescribeAnomalyDetectors = DescribeAnomalyDetectorsResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DescribeAnomalyDetectorsResult"
      ( \s h x ->
          DescribeAnomalyDetectorsResponse'
            Lude.<$> ( x Lude..@? "AnomalyDetectors" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAnomalyDetectors where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAnomalyDetectors where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAnomalyDetectors where
  toQuery DescribeAnomalyDetectors' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAnomalyDetectors" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "NextToken" Lude.=: nextToken,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeAnomalyDetectorsResponse' smart constructor.
data DescribeAnomalyDetectorsResponse = DescribeAnomalyDetectorsResponse'
  { anomalyDetectors ::
      Lude.Maybe
        [AnomalyDetector],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAnomalyDetectorsResponse' with the minimum fields required to make a request.
--
-- * 'anomalyDetectors' - The list of anomaly detection models returned by the operation.
-- * 'nextToken' - A token that you can use in a subsequent operation to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeAnomalyDetectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAnomalyDetectorsResponse
mkDescribeAnomalyDetectorsResponse pResponseStatus_ =
  DescribeAnomalyDetectorsResponse'
    { anomalyDetectors =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of anomaly detection models returned by the operation.
--
-- /Note:/ Consider using 'anomalyDetectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrsAnomalyDetectors :: Lens.Lens' DescribeAnomalyDetectorsResponse (Lude.Maybe [AnomalyDetector])
dadrsAnomalyDetectors = Lens.lens (anomalyDetectors :: DescribeAnomalyDetectorsResponse -> Lude.Maybe [AnomalyDetector]) (\s a -> s {anomalyDetectors = a} :: DescribeAnomalyDetectorsResponse)
{-# DEPRECATED dadrsAnomalyDetectors "Use generic-lens or generic-optics with 'anomalyDetectors' instead." #-}

-- | A token that you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrsNextToken :: Lens.Lens' DescribeAnomalyDetectorsResponse (Lude.Maybe Lude.Text)
dadrsNextToken = Lens.lens (nextToken :: DescribeAnomalyDetectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAnomalyDetectorsResponse)
{-# DEPRECATED dadrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrsResponseStatus :: Lens.Lens' DescribeAnomalyDetectorsResponse Lude.Int
dadrsResponseStatus = Lens.lens (responseStatus :: DescribeAnomalyDetectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAnomalyDetectorsResponse)
{-# DEPRECATED dadrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
