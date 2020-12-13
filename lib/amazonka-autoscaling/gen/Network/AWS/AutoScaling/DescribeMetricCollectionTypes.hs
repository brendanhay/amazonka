{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available CloudWatch metrics for Amazon EC2 Auto Scaling.
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must explicitly request this metric when calling the 'EnableMetricsCollection' API.
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
  ( -- * Creating a request
    DescribeMetricCollectionTypes (..),
    mkDescribeMetricCollectionTypes,

    -- * Destructuring the response
    DescribeMetricCollectionTypesResponse (..),
    mkDescribeMetricCollectionTypesResponse,

    -- ** Response lenses
    dmctrsMetrics,
    dmctrsGranularities,
    dmctrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMetricCollectionTypes' with the minimum fields required to make a request.
mkDescribeMetricCollectionTypes ::
  DescribeMetricCollectionTypes
mkDescribeMetricCollectionTypes = DescribeMetricCollectionTypes'

instance Lude.AWSRequest DescribeMetricCollectionTypes where
  type
    Rs DescribeMetricCollectionTypes =
      DescribeMetricCollectionTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeMetricCollectionTypesResult"
      ( \s h x ->
          DescribeMetricCollectionTypesResponse'
            Lude.<$> ( x Lude..@? "Metrics" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "Granularities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMetricCollectionTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeMetricCollectionTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMetricCollectionTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeMetricCollectionTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeMetricCollectionTypesResponse' smart constructor.
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
  { -- | One or more metrics.
    metrics :: Lude.Maybe [MetricCollectionType],
    -- | The granularities for the metrics.
    granularities :: Lude.Maybe [MetricGranularityType],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMetricCollectionTypesResponse' with the minimum fields required to make a request.
--
-- * 'metrics' - One or more metrics.
-- * 'granularities' - The granularities for the metrics.
-- * 'responseStatus' - The response status code.
mkDescribeMetricCollectionTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMetricCollectionTypesResponse
mkDescribeMetricCollectionTypesResponse pResponseStatus_ =
  DescribeMetricCollectionTypesResponse'
    { metrics = Lude.Nothing,
      granularities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One or more metrics.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrsMetrics :: Lens.Lens' DescribeMetricCollectionTypesResponse (Lude.Maybe [MetricCollectionType])
dmctrsMetrics = Lens.lens (metrics :: DescribeMetricCollectionTypesResponse -> Lude.Maybe [MetricCollectionType]) (\s a -> s {metrics = a} :: DescribeMetricCollectionTypesResponse)
{-# DEPRECATED dmctrsMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The granularities for the metrics.
--
-- /Note:/ Consider using 'granularities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrsGranularities :: Lens.Lens' DescribeMetricCollectionTypesResponse (Lude.Maybe [MetricGranularityType])
dmctrsGranularities = Lens.lens (granularities :: DescribeMetricCollectionTypesResponse -> Lude.Maybe [MetricGranularityType]) (\s a -> s {granularities = a} :: DescribeMetricCollectionTypesResponse)
{-# DEPRECATED dmctrsGranularities "Use generic-lens or generic-optics with 'granularities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrsResponseStatus :: Lens.Lens' DescribeMetricCollectionTypesResponse Lude.Int
dmctrsResponseStatus = Lens.lens (responseStatus :: DescribeMetricCollectionTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMetricCollectionTypesResponse)
{-# DEPRECATED dmctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
