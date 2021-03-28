{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
  ( PredefinedMetricSpecification (..)
  -- * Smart constructor
  , mkPredefinedMetricSpecification
  -- * Lenses
  , pmsPredefinedMetricType
  , pmsResourceLabel
  ) where

import qualified Network.AWS.AutoScaling.Types.MetricType as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen1023 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a predefined metric for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
-- /See:/ 'mkPredefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'
  { predefinedMetricType :: Types.MetricType
    -- ^ The metric type. The following predefined metrics are available:
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
  , resourceLabel :: Core.Maybe Types.XmlStringMaxLen1023
    -- ^ Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Auto Scaling group.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PredefinedMetricSpecification' value with any optional fields omitted.
mkPredefinedMetricSpecification
    :: Types.MetricType -- ^ 'predefinedMetricType'
    -> PredefinedMetricSpecification
mkPredefinedMetricSpecification predefinedMetricType
  = PredefinedMetricSpecification'{predefinedMetricType,
                                   resourceLabel = Core.Nothing}

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
pmsPredefinedMetricType :: Lens.Lens' PredefinedMetricSpecification Types.MetricType
pmsPredefinedMetricType = Lens.field @"predefinedMetricType"
{-# INLINEABLE pmsPredefinedMetricType #-}
{-# DEPRECATED predefinedMetricType "Use generic-lens or generic-optics with 'predefinedMetricType' instead"  #-}

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
pmsResourceLabel :: Lens.Lens' PredefinedMetricSpecification (Core.Maybe Types.XmlStringMaxLen1023)
pmsResourceLabel = Lens.field @"resourceLabel"
{-# INLINEABLE pmsResourceLabel #-}
{-# DEPRECATED resourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead"  #-}

instance Core.ToQuery PredefinedMetricSpecification where
        toQuery PredefinedMetricSpecification{..}
          = Core.toQueryPair "PredefinedMetricType" predefinedMetricType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResourceLabel")
                resourceLabel

instance Core.FromXML PredefinedMetricSpecification where
        parseXML x
          = PredefinedMetricSpecification' Core.<$>
              (x Core..@ "PredefinedMetricType") Core.<*>
                x Core..@? "ResourceLabel"
