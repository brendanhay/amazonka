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
    plmsResourceLabel,
    plmsPredefinedLoadMetricType,
  )
where

import Network.AWS.AutoScalingPlans.Types.LoadMetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a predefined metric that can be used for predictive scaling.
--
-- /See:/ 'mkPredefinedLoadMetricSpecification' smart constructor.
data PredefinedLoadMetricSpecification = PredefinedLoadMetricSpecification'
  { -- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group.
    --
    -- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
    --
    --     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
    --
    --
    --     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
    resourceLabel :: Lude.Maybe Lude.Text,
    -- | The metric type.
    predefinedLoadMetricType :: LoadMetricType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PredefinedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- * 'resourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group.
--
-- The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:
--
--     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.
--
--
--     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
--
-- * 'predefinedLoadMetricType' - The metric type.
mkPredefinedLoadMetricSpecification ::
  -- | 'predefinedLoadMetricType'
  LoadMetricType ->
  PredefinedLoadMetricSpecification
mkPredefinedLoadMetricSpecification pPredefinedLoadMetricType_ =
  PredefinedLoadMetricSpecification'
    { resourceLabel = Lude.Nothing,
      predefinedLoadMetricType = pPredefinedLoadMetricType_
    }

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
plmsResourceLabel :: Lens.Lens' PredefinedLoadMetricSpecification (Lude.Maybe Lude.Text)
plmsResourceLabel = Lens.lens (resourceLabel :: PredefinedLoadMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {resourceLabel = a} :: PredefinedLoadMetricSpecification)
{-# DEPRECATED plmsResourceLabel "Use generic-lens or generic-optics with 'resourceLabel' instead." #-}

-- | The metric type.
--
-- /Note:/ Consider using 'predefinedLoadMetricType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plmsPredefinedLoadMetricType :: Lens.Lens' PredefinedLoadMetricSpecification LoadMetricType
plmsPredefinedLoadMetricType = Lens.lens (predefinedLoadMetricType :: PredefinedLoadMetricSpecification -> LoadMetricType) (\s a -> s {predefinedLoadMetricType = a} :: PredefinedLoadMetricSpecification)
{-# DEPRECATED plmsPredefinedLoadMetricType "Use generic-lens or generic-optics with 'predefinedLoadMetricType' instead." #-}

instance Lude.FromJSON PredefinedLoadMetricSpecification where
  parseJSON =
    Lude.withObject
      "PredefinedLoadMetricSpecification"
      ( \x ->
          PredefinedLoadMetricSpecification'
            Lude.<$> (x Lude..:? "ResourceLabel")
            Lude.<*> (x Lude..: "PredefinedLoadMetricType")
      )

instance Lude.ToJSON PredefinedLoadMetricSpecification where
  toJSON PredefinedLoadMetricSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceLabel" Lude..=) Lude.<$> resourceLabel,
            Lude.Just
              ("PredefinedLoadMetricType" Lude..= predefinedLoadMetricType)
          ]
      )
