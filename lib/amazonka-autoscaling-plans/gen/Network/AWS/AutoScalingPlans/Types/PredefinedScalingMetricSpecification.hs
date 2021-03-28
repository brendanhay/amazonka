{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
  ( PredefinedScalingMetricSpecification (..)
  -- * Smart constructor
  , mkPredefinedScalingMetricSpecification
  -- * Lenses
  , psmsPredefinedScalingMetricType
  , psmsResourceLabel
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.ResourceLabel as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingMetricType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a predefined metric that can be used for dynamic scaling as part of a target tracking scaling policy.
--
-- /See:/ 'mkPredefinedScalingMetricSpecification' smart constructor.
data PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification'
  { predefinedScalingMetricType :: Types.ScalingMetricType
    -- ^ The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
  , resourceLabel :: Core.Maybe Types.ResourceLabel
    -- ^ Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service.
--
-- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PredefinedScalingMetricSpecification' value with any optional fields omitted.
mkPredefinedScalingMetricSpecification
    :: Types.ScalingMetricType -- ^ 'predefinedScalingMetricType'
    -> PredefinedScalingMetricSpecification
mkPredefinedScalingMetricSpecification predefinedScalingMetricType
  = PredefinedScalingMetricSpecification'{predefinedScalingMetricType,
                                          resourceLabel = Core.Nothing}

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
--
-- /Note:/ Consider using 'predefinedScalingMetricType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psmsPredefinedScalingMetricType :: Lens.Lens' PredefinedScalingMetricSpecification Types.ScalingMetricType
psmsPredefinedScalingMetricType = Lens.field @"predefinedScalingMetricType"
{-# INLINEABLE psmsPredefinedScalingMetricType #-}
{-# DEPRECATED predefinedScalingMetricType "Use generic-lens or generic-optics with 'predefinedScalingMetricType' instead"  #-}

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service.
--
-- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
--
--
-- /Note:/ Consider using 'resourceLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psmsResourceLabel :: Lens.Lens' PredefinedScalingMetricSpecification (Core.Maybe Types.ResourceLabel)
psmsResourceLabel = Lens.field @"resourceLabel"
{-# INLINEABLE psmsResourceLabel #-}
{-# DEPRECATED resourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead"  #-}

instance Core.FromJSON PredefinedScalingMetricSpecification where
        toJSON PredefinedScalingMetricSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("PredefinedScalingMetricType" Core..=
                       predefinedScalingMetricType),
                  ("ResourceLabel" Core..=) Core.<$> resourceLabel])

instance Core.FromJSON PredefinedScalingMetricSpecification where
        parseJSON
          = Core.withObject "PredefinedScalingMetricSpecification" Core.$
              \ x ->
                PredefinedScalingMetricSpecification' Core.<$>
                  (x Core..: "PredefinedScalingMetricType") Core.<*>
                    x Core..:? "ResourceLabel"
