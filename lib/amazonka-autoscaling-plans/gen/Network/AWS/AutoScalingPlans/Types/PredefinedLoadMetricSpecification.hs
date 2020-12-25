{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
  ( PredefinedLoadMetricSpecification (..),

    -- * Smart constructor
    mkPredefinedLoadMetricSpecification,

    -- * Lenses
    plmsPredefinedLoadMetricType,
    plmsResourceLabel,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types.LoadMetricType as Types
import qualified Network.AWS.AutoScalingPlans.Types.ResourceLabel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a predefined metric that can be used for predictive scaling.
--
-- /See:/ 'mkPredefinedLoadMetricSpecification' smart constructor.
data PredefinedLoadMetricSpecification = PredefinedLoadMetricSpecification'
  { -- | The metric type.
    predefinedLoadMetricType :: Types.LoadMetricType,
    -- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group.
    --
    -- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
    --
    --     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
    --
    --
    --     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
    resourceLabel :: Core.Maybe Types.ResourceLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PredefinedLoadMetricSpecification' value with any optional fields omitted.
mkPredefinedLoadMetricSpecification ::
  -- | 'predefinedLoadMetricType'
  Types.LoadMetricType ->
  PredefinedLoadMetricSpecification
mkPredefinedLoadMetricSpecification predefinedLoadMetricType =
  PredefinedLoadMetricSpecification'
    { predefinedLoadMetricType,
      resourceLabel = Core.Nothing
    }

-- | The metric type.
--
-- /Note:/ Consider using 'predefinedLoadMetricType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plmsPredefinedLoadMetricType :: Lens.Lens' PredefinedLoadMetricSpecification Types.LoadMetricType
plmsPredefinedLoadMetricType = Lens.field @"predefinedLoadMetricType"
{-# DEPRECATED plmsPredefinedLoadMetricType "Use generic-lens or generic-optics with 'predefinedLoadMetricType' instead." #-}

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group.
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
plmsResourceLabel :: Lens.Lens' PredefinedLoadMetricSpecification (Core.Maybe Types.ResourceLabel)
plmsResourceLabel = Lens.field @"resourceLabel"
{-# DEPRECATED plmsResourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead." #-}

instance Core.FromJSON PredefinedLoadMetricSpecification where
  toJSON PredefinedLoadMetricSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PredefinedLoadMetricType" Core..= predefinedLoadMetricType),
            ("ResourceLabel" Core..=) Core.<$> resourceLabel
          ]
      )

instance Core.FromJSON PredefinedLoadMetricSpecification where
  parseJSON =
    Core.withObject "PredefinedLoadMetricSpecification" Core.$
      \x ->
        PredefinedLoadMetricSpecification'
          Core.<$> (x Core..: "PredefinedLoadMetricType")
          Core.<*> (x Core..:? "ResourceLabel")
