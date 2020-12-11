-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
  ( PredefinedScalingMetricSpecification (..),

    -- * Smart constructor
    mkPredefinedScalingMetricSpecification,

    -- * Lenses
    psmsResourceLabel,
    psmsPredefinedScalingMetricType,
  )
where

import Network.AWS.AutoScalingPlans.Types.ScalingMetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a predefined metric that can be used for dynamic scaling as part of a target tracking scaling policy.
--
-- /See:/ 'mkPredefinedScalingMetricSpecification' smart constructor.
data PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification'
  { resourceLabel ::
      Lude.Maybe
        Lude.Text,
    predefinedScalingMetricType ::
      ScalingMetricType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PredefinedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- * 'predefinedScalingMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
-- * 'resourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service.
--
-- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
mkPredefinedScalingMetricSpecification ::
  -- | 'predefinedScalingMetricType'
  ScalingMetricType ->
  PredefinedScalingMetricSpecification
mkPredefinedScalingMetricSpecification
  pPredefinedScalingMetricType_ =
    PredefinedScalingMetricSpecification'
      { resourceLabel =
          Lude.Nothing,
        predefinedScalingMetricType =
          pPredefinedScalingMetricType_
      }

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
psmsResourceLabel :: Lens.Lens' PredefinedScalingMetricSpecification (Lude.Maybe Lude.Text)
psmsResourceLabel = Lens.lens (resourceLabel :: PredefinedScalingMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {resourceLabel = a} :: PredefinedScalingMetricSpecification)
{-# DEPRECATED psmsResourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead." #-}

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
--
-- /Note:/ Consider using 'predefinedScalingMetricType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psmsPredefinedScalingMetricType :: Lens.Lens' PredefinedScalingMetricSpecification ScalingMetricType
psmsPredefinedScalingMetricType = Lens.lens (predefinedScalingMetricType :: PredefinedScalingMetricSpecification -> ScalingMetricType) (\s a -> s {predefinedScalingMetricType = a} :: PredefinedScalingMetricSpecification)
{-# DEPRECATED psmsPredefinedScalingMetricType "Use generic-lens or generic-optics with 'predefinedScalingMetricType' instead." #-}

instance Lude.FromJSON PredefinedScalingMetricSpecification where
  parseJSON =
    Lude.withObject
      "PredefinedScalingMetricSpecification"
      ( \x ->
          PredefinedScalingMetricSpecification'
            Lude.<$> (x Lude..:? "ResourceLabel")
            Lude.<*> (x Lude..: "PredefinedScalingMetricType")
      )

instance Lude.ToJSON PredefinedScalingMetricSpecification where
  toJSON PredefinedScalingMetricSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceLabel" Lude..=) Lude.<$> resourceLabel,
            Lude.Just
              ( "PredefinedScalingMetricType"
                  Lude..= predefinedScalingMetricType
              )
          ]
      )
