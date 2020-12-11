-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
  ( PredefinedMetricSpecification (..),

    -- * Smart constructor
    mkPredefinedMetricSpecification,

    -- * Lenses
    pmsResourceLabel,
    pmsPredefinedMetricType,
  )
where

import Network.AWS.AutoScaling.Types.MetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a predefined metric for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
-- /See:/ 'mkPredefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'
  { resourceLabel ::
      Lude.Maybe Lude.Text,
    predefinedMetricType ::
      MetricType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PredefinedMetricSpecification' with the minimum fields required to make a request.
--
-- * 'predefinedMetricType' - The metric type. The following predefined metrics are available:
--
--
--     * @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto Scaling group.
--
--
--     * @ASGAverageNetworkIn@ - Average number of bytes received on all network interfaces by the Auto Scaling group.
--
--
--     * @ASGAverageNetworkOut@ - Average number of bytes sent out on all network interfaces by the Auto Scaling group.
--
--
--     * @ALBRequestCountPerTarget@ - Number of requests completed per target in an Application Load Balancer target group.
--
--
-- * 'resourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Auto Scaling group.
--
-- You create the resource label by appending the final portion of the load balancer ARN and the final portion of the target group ARN into a single value, separated by a forward slash (/). The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
--
-- This is an example: app/EC2Co-EcsEl-1TKLTMITMM0EO/f37c06a68c1748aa/targetgroup/EC2Co-Defau-LDNM7Q3ZH1ZN/6d4ea56ca2d6a18d.
-- To find the ARN for an Application Load Balancer, use the <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> API operation. To find the ARN for the target group, use the <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
mkPredefinedMetricSpecification ::
  -- | 'predefinedMetricType'
  MetricType ->
  PredefinedMetricSpecification
mkPredefinedMetricSpecification pPredefinedMetricType_ =
  PredefinedMetricSpecification'
    { resourceLabel = Lude.Nothing,
      predefinedMetricType = pPredefinedMetricType_
    }

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Auto Scaling group.
--
-- You create the resource label by appending the final portion of the load balancer ARN and the final portion of the target group ARN into a single value, separated by a forward slash (/). The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
--
-- This is an example: app/EC2Co-EcsEl-1TKLTMITMM0EO/f37c06a68c1748aa/targetgroup/EC2Co-Defau-LDNM7Q3ZH1ZN/6d4ea56ca2d6a18d.
-- To find the ARN for an Application Load Balancer, use the <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> API operation. To find the ARN for the target group, use the <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
--
-- /Note:/ Consider using 'resourceLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmsResourceLabel :: Lens.Lens' PredefinedMetricSpecification (Lude.Maybe Lude.Text)
pmsResourceLabel = Lens.lens (resourceLabel :: PredefinedMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {resourceLabel = a} :: PredefinedMetricSpecification)
{-# DEPRECATED pmsResourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead." #-}

-- | The metric type. The following predefined metrics are available:
--
--
--     * @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto Scaling group.
--
--
--     * @ASGAverageNetworkIn@ - Average number of bytes received on all network interfaces by the Auto Scaling group.
--
--
--     * @ASGAverageNetworkOut@ - Average number of bytes sent out on all network interfaces by the Auto Scaling group.
--
--
--     * @ALBRequestCountPerTarget@ - Number of requests completed per target in an Application Load Balancer target group.
--
--
--
-- /Note:/ Consider using 'predefinedMetricType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmsPredefinedMetricType :: Lens.Lens' PredefinedMetricSpecification MetricType
pmsPredefinedMetricType = Lens.lens (predefinedMetricType :: PredefinedMetricSpecification -> MetricType) (\s a -> s {predefinedMetricType = a} :: PredefinedMetricSpecification)
{-# DEPRECATED pmsPredefinedMetricType "Use generic-lens or generic-optics with 'predefinedMetricType' instead." #-}

instance Lude.FromXML PredefinedMetricSpecification where
  parseXML x =
    PredefinedMetricSpecification'
      Lude.<$> (x Lude..@? "ResourceLabel")
      Lude.<*> (x Lude..@ "PredefinedMetricType")

instance Lude.ToQuery PredefinedMetricSpecification where
  toQuery PredefinedMetricSpecification' {..} =
    Lude.mconcat
      [ "ResourceLabel" Lude.=: resourceLabel,
        "PredefinedMetricType" Lude.=: predefinedMetricType
      ]
